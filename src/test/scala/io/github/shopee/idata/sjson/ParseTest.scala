package io.github.free.lock.sjson

import scala.collection.mutable.ListBuffer

case class User(name: String, email: String)
case class Users(users: List[User])
case class Person(name: String, gender: String)
case class CommandData(text: String = "", errno: Int = 0, errMsg: String = "")

class ParseTest extends org.scalatest.FunSuite {
  private def testParseSym(v: Any) {
    assert(JSON.parse(JSON.stringify(v)) == v)
  }

  test("parse: map unicode") {
    val input = "[{\"\\u\":\"\\u\"}]"
    val output = JSON.parse(input).asInstanceOf[List[Map[String, String]]](0)("\\u")
    assert(output ==  "\\u")
  }

  test("parse: map unicode2") {
    val input = "[{\"\\uD83\":\"\\uD83\"}]"
    val output = JSON.parse(input).asInstanceOf[List[Map[String, String]]](0)("\\uD83")
    assert(output ==  "\\uD83")
  }

  test("parse: map unicode3") {
    val input = "[{\"\\uD835\\uDC07\":\"\\uD835\\uDC07\"}]"
    //𝐇
    val output = JSON.parse(input).asInstanceOf[List[Map[String, String]]](0)("\uD835\uDC07")
    assert(output ==  "\uD835\uDC07")
  }

  test("parse: map unicode4") {
    val input = "[{\"シリアライゼーション\":\"シリアライゼーション\"}]"
    val output = JSON.parse(input).asInstanceOf[List[Map[String, String]]](0)("シリアライゼーション")
    assert(output ==  "シリアライゼーション")
  }

  test("parse: map unicode5") {
    val input = "[{\"\\uD835\\uDC07\":\"\\uD835\\uDC07\"}]"
    //𝐇
    val output = JSON.parse(input).asInstanceOf[List[Map[String, String]]](0)("\uD835\uDC07")
    assert(output ==  "\uD835\uDC07")
  }

  test("parse: true|false|null") {
    List[Any](true, false, null).map(testParseSym)
    assert(JSON.parse(JSON.stringify(None)) == null)
  }

  test("parse: number") {
    List[Any](0, -0, 123, 0.123, 1.234, 1874658600483736L).map(testParseSym)
    assert(JSON.parse("1.234E0") == 1.234)
    assert(JSON.parse("0E-8") == 0) // big decimal
    assert(JSON.parse("92233720368547758").isInstanceOf[Long])
  }

  test("parse: string") {
    List[Any]("", "hello, world", "a\"b", "a\nb", "a\"\nb").map(testParseSym)
  }

  test("parse: map") {
    List[Any](Map(), Map("a" -> 1)).map(testParseSym)
  }

  test("parse: parse replacer") {
    assert(JSON.parse(JSON.stringify(1), (data, _, _) => {
      data.asInstanceOf[Int] + 1
    }) == 2)

    assert(JSON.parse(JSON.stringify(List(1, 2, null)), (data, _, _) => {
      if (data == null) 0 else data
    }) == List(1, 2, 0))
  }

  test("parse-async: base") {
    val textIter  = new AsyncIterator[Char]()
    val parseIter = JSON.parseAsyncIterator(textIter)
    var count     = 0
    parseIter.forEach((item, index) => {
      count += 1
      assert(item == 1)
      item
    })

    textIter.push('1')
    textIter.end()
    assert(count == 1)
  }

  test("parse-async: list") {
    val textIter  = new AsyncIterator[Char]()
    val parseIter = JSON.parseAsyncIterator(textIter)
    var count     = 0
    parseIter.forEach((item, index) => {
      count += 1
      if (index < 3) assert(item == count) else assert(item == List(1, 2, 3))
      item
    })

    textIter.push('[')
    textIter.push('1')
    textIter.push(',')
    textIter.push('2')
    textIter.push(',')
    textIter.push('3')
    textIter.push(']')
    textIter.end()
    assert(count == 4)
  }

  test("parse-async: drop in stream example") {
    val textIter = new AsyncIterator[Char]()
    val list     = ListBuffer[Any]()

    var index = 0
    val parseIter = JSON.parseAsyncIterator(
      textIter,
      (data, pathStack, _) => {
        if (pathStack.length == 2 && pathStack(pathStack.length - 1).index == "data" && pathStack(
              pathStack.length - 2
            ).ntype == PathNode.ARRAY_CTX) {
          assert(JSON.toJsonPath(pathStack) == "data.[0]")
          index += 1
          list.append(data)
          JSON.WIPE_VALUE
        } else data
      }
    )

    var result: Any = null

    parseIter
      .reduce[Any]((item, index, prev) => item, null, ResultCallback(endCallback = (prev) => {
        result = prev
      }))

    val datas = List(Map(
                       "a" -> 1,
                       "b" -> 2
                     ),
                     Map("a" -> 10, "b" -> 20),
                     Map(
                       "a" -> 12,
                       "b" -> 123
                     ))
    textIter.pushList(
      JSON
        .stringify(
          Map(
            "type" -> "normal",
            "data" -> datas, // imagine this list is very huge, and received from the network
            "msg"  -> "some"
          )
        )
        .toList
    )

    textIter.end()

    assert(result == Map("type" -> "normal", "data" -> List(), "msg" -> "some"))
    assert(list == datas)
  }

  import scala.reflect.ClassTag
  import scala.reflect._
  import scala.reflect.runtime.universe._

  def revert[T: TypeTag](value: T) = {
    val txt = JSON.stringify(value)
    val ret = JSON.parseTo[T](txt)
    assert(ret == value)
  }

  test("parse: case class") {
    import reflect.runtime.universe._
    import JSONConverter._

    revert(1000l)
    revert("1234")
    revert(1234)
    revert(true)
    revert(false)
    revert(null)
    revert(List[String]("hello"))
    revert(List[Int](1, 2))
    revert(List[Any](1, 2, "hello"))
    revert(Map[String, Double]("a" -> 1, "b"       -> 2))
    revert(Map[String, Any]("a"    -> 1, "b"       -> 2))
    revert(Map[String, String]("a" -> "hello", "b" -> "world!"))

    revert(Person("ddchen", "male"))
    revert(CommandData())
    revert(Users(List(User("arre0", "arre0.com"), User("arre1", "arre1.com"))))
  }
}
