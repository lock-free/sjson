package io.github.shopee.idata.sjson

case class User(name: String, email: String)
case class Users(users: List[User])
case class Person(name: String, gender: String)
case class CommandData(text: String = "", errno: Int = 0, errMsg: String = "")

class ParseTest extends org.scalatest.FunSuite {
  private def testParseSym(v: Any) {
    assert(JSON.parse(JSON.stringify(v)) == v)
  }

  test("parse: true|false|null") {
    List[Any](true, false, null).map(testParseSym)
    assert(JSON.parse(JSON.stringify(None)) == null)
  }

  test("parse: number") {
    List[Any](0, -0, 123, 0.123, 1.234, 1874658600483736L).map(testParseSym)
    assert(JSON.parse("1.234E0") == 1.234)
    assert(JSON.parse("0E-8") == 0) // big decimal
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
      if(data == null) 0 else data
    }) == List(1, 2, 0))
  }

  test("parse-async: base") {
    val textIter = new AsyncIterator[Char]()
    val parseIter = JSON.parseAsyncIterator(textIter)
    var count = 0
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
    val textIter = new AsyncIterator[Char]()
    val parseIter = JSON.parseAsyncIterator(textIter)
    var count = 0
    parseIter.forEach((item, index) => {
      count += 1
      if(index < 4) assert(item == count) else assert(item ==List(1,2,3))
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
    val parseIter = JSON.parseAsyncIterator(textIter, (data, stack, _) => {
      println(data)
      println(stack)
      if(stack.length == 6) {
        JSON.WIPE_VALUE
      } else data
    })
    parseIter.forEach()

    textIter.pushList(JSON.stringify(Map(
      "type" -> "normal",
      "data"-> List(Map(
        "a"-> 1,
        "b" -> 2
       ), Map(
         "a"-> 10,
        "b" -> 20)), // imagine this list is very huge, and received from the network
      "msg" -> "some"
      )).toList)

    textIter.end()
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
