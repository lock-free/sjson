package io.github.free.lock.sjson

import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

object AsyncIterator {
  val DATA  = 0 // stream data
  val END   = 1 // stream end
  val ERROR = 2 // stream stopped unexpected

  // item, index, prev
  type ItemHandler[T, U] = (T, Int, U) => U
  type ShouldStop[T]     = (T, Int) => Boolean
  type EndCallback[U]    = (U) => Unit
  type ErrorCallback     = (Exception) => Unit

  def defaultShouldStop[T](v: T, index: Int) = false
}

case class ResultCallback[U](endCallback: AsyncIterator.EndCallback[U] = null,
                             errorCallback: AsyncIterator.ErrorCallback = null)

case class NextHandler[T, U](itemHandler: AsyncIterator.ItemHandler[T, U],
                             resultCallback: ResultCallback[U])

class AsyncIterator[T]() {
  private var index               = -1 // current data index
  private var buffer: Queue[T]    = Queue[T]()
  private var state               = AsyncIterator.DATA
  private var errorObj: Exception = null

  private var nextHandler: NextHandler[T, Any] = null

  def next(handler: NextHandler[T, Any]) =
    if (state == AsyncIterator.END) {
      handler.resultCallback.endCallback(null)
    } else if (state == AsyncIterator.ERROR) {
      handler.resultCallback.errorCallback(errorObj)
    } else {
      if (buffer.isEmpty) {
        nextHandler = handler
      } else {
        index += 1
        handler.itemHandler(buffer.dequeue(), index, null)
      }
    }

  def push(item: T) {
    if (state != AsyncIterator.DATA) {
      throw new Exception(
        s"current state of iterator is not DATA. Current state is ${AsyncIterator.DATA}."
      )
    }
    if (nextHandler != null) {
      val oldNextHandler = nextHandler
      nextHandler = null
      index += 1
      oldNextHandler.itemHandler(item, index, null)
    } else {
      buffer.enqueue(item)
    }
  }

  def pushList(items: List[T]) {
    for (item <- items) push(item)
  }

  def end() {
    state = AsyncIterator.END
    index += 1
    if (nextHandler != null) {
      val oldNextHandler = nextHandler
      nextHandler = null
      oldNextHandler.resultCallback.endCallback(null)
    }
  }

  def error(err: Exception) {
    state = AsyncIterator.ERROR
    errorObj = err
    if (nextHandler != null) {
      val oldNextHandler = nextHandler
      nextHandler = null
      oldNextHandler.resultCallback.errorCallback(errorObj)
    }
  }

  /********************************************************
    * combinators for async iterator
   ********************************************************/
  /**
    * general process to the stream, one by one
    */
  def process[U](itemHandler: AsyncIterator.ItemHandler[T, U],
                 shouldStop: AsyncIterator.ShouldStop[T] = AsyncIterator.defaultShouldStop,
                 resultCallback: ResultCallback[U] = null,
                 prev: U = null): Unit =
    next(
      NextHandler(
        (v, index, p) => {
          val nextPrev = itemHandler(v, index, prev)
          if (shouldStop(v, index)) { // stop
            if (resultCallback != null && resultCallback.endCallback != null)
              resultCallback.endCallback(nextPrev)
          } else {
            // keep going
            process(itemHandler, shouldStop, resultCallback, nextPrev)
          }
        },
        resultCallback = ResultCallback(
          (_) => {
            if (resultCallback != null && resultCallback.endCallback != null)
              resultCallback.endCallback(prev)
          },
          (errorObj) => {
            if (resultCallback != null && resultCallback.errorCallback != null) {
              resultCallback.errorCallback(errorObj)
            } else {
              throw errorObj
            }
          }
        )
      )
    )

  def forEach(itemHandler: (T, Int) => Unit = null, resultCallback: ResultCallback[Null] = null) =
    process[Null](
      itemHandler = (v, index, prev) => {
        if (itemHandler != null) itemHandler(v, index)
        prev
      },
      resultCallback = resultCallback,
      prev = null
    )

  def reduce[U](itemHandler: AsyncIterator.ItemHandler[T, U],
                prev: U,
                resultCallback: ResultCallback[U] = null) =
    process[U](
      itemHandler = itemHandler,
      resultCallback = resultCallback,
      prev = prev
    )

  def take(num: Int, resultCallback: ResultCallback[ListBuffer[T]]) = {
    val firstIndex = index

    process[ListBuffer[T]](
      itemHandler = (v, index, prev) => {
        prev.append(v)
        prev
      },
      shouldStop = (v, index) => index - firstIndex >= num,
      resultCallback = resultCallback,
      ListBuffer[T]()
    )
  }

  def getIndex() = index
}
