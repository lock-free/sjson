package io.github.shopee.idata.sjson

class AsyncIteratorTest extends org.scalatest.FunSuite {
  private def sumIter(iter: AsyncIterator[Int], getRet: (Int) => Unit, prev: Int = 0) {
    iter.next((v) => {
      v match {
        case None => getRet(prev)
        case Some(r) => sumIter(iter, getRet, prev + r)
      }
    })
  }

  test("base") {
    val iter = new AsyncIterator[Int]()
    var sum = 0
    sumIter(iter, (v) => {
      sum = v
    })
    iter.push(1)
    iter.push(5)
    iter.end()
    assert(sum == 6)
  }

  test("base2") {
    val iter = new AsyncIterator[Int]()
    iter.push(1)
    iter.push(5)
    var sum = 0
    sumIter(iter, (v) => {
      sum = v
    })

    iter.end()
    assert(sum == 6)
  }

  test("async") {
    val iter = new AsyncIterator[Int]()
    
    var sum = 0
    sumIter(iter, (v) => {
      sum = v
    })

    Thread.sleep(10)
    iter.push(1)
    iter.push(5)
    iter.end()
    assert(sum == 6)
  }
}
