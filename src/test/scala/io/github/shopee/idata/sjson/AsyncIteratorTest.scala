package io.github.shopee.idata.sjson

class AsyncIteratorTest extends org.scalatest.FunSuite {
  private def sumIter(iter: AsyncIterator[Int], getRet: (Int) => Unit) {
    iter.reduce[Int]((v, index, prev) => v + prev,
                     0,
                     resultCallback = ResultCallback(endCallback = (sum) => getRet(sum)))
  }

  test("base") {
    val iter = new AsyncIterator[Int]()
    var sum  = 0
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

  test("index") {
    val iter = new AsyncIterator[Int]()
    var ei   = 0
    iter.process[Any]((v, index, prev) => {
      assert(ei == index)
      ei += 1
    })

    iter.push(1)
    iter.push(12)
    iter.push(-10)
    iter.end()
  }

  test("take: async") {
    val iter = new AsyncIterator[Int]()
    var ei   = 0
    iter.take(2, ResultCallback((list) => {
      assert(list.toList == List(1, 12))
    }))

    iter.push(1)
    iter.push(12)
    iter.push(-10)
    iter.end()
  }

  test("take: queue") {
    val iter = new AsyncIterator[Int]()
    var ei   = 0

    iter.push(1)
    iter.push(12)
    iter.push(-10)
    iter.take(2, ResultCallback((list) => {
      assert(list.toList == List(1, 12))
    }))
    iter.end()
  }

  test("take: too much") {
    val iter = new AsyncIterator[Int]()
    var ei   = 0
    iter.take(100, ResultCallback((list) => {
      assert(list.toList == List(1, 12, -10))
    }))

    iter.push(1)
    iter.push(12)
    iter.push(-10)
    iter.end()
  }

  test("take: continue") {
    val iter = new AsyncIterator[Int]()
    var ei   = 0

    iter.push(1)
    iter.push(12)
    iter.push(-10)
    iter.take(
      1,
      ResultCallback((list) => {
        assert(list.toList == List(1))
        iter.take(
          1,
          ResultCallback((list) => {
            assert(list.toList == List(12))
            iter.take(1, ResultCallback((list) => {
              assert(list.toList == List(-10))
              iter.take(1, ResultCallback((list) => {
                assert(list.toList == List())
              }))
            }))
          })
        )
      })
    )
    iter.end()
  }
}
