package io.github.shopee.idata.sjson

import scala.collection.mutable.Queue

object AsyncIterator {
  type OnNext[T] = (Option[T]) => Unit
}

class AsyncIterator[T]() {
  private var buffer: Queue[T] = Queue[T]()
  private var isEnd = false
  private var nextHandler: AsyncIterator.OnNext[T] = null

  def next(onNext: AsyncIterator.OnNext[T]) = {
    if(isEnd) {
      onNext(None)
    } else if(buffer.isEmpty) {
      nextHandler = onNext // need to wait
    } else {
      onNext(Some(buffer.dequeue))
    }
  }

  def push(item: T) {
    if(nextHandler != null) {
      val oldNextHandler = nextHandler
      nextHandler = null
      oldNextHandler(Some(item))
    } else {
      buffer.enqueue(item)
    }
  }

  def end() {
    isEnd = true
    if(nextHandler != null) {
      val oldNextHandler = nextHandler
      nextHandler = null
      oldNextHandler(None)
    }
  }
}
