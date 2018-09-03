package io.github.shopee.idata.sjson

case class User(name: String, email: String)
case class Users(users: List[User])
case class Person(name: String, gender: String)
case class CommandData(text: String = "", errno: Int = 0, errMsg: String = "")

class ParseTest extends org.scalatest.FunSuite {
  test("parse: true|false|null") {
    assert(JSON.parse(JSON.stringify(true)) == true)
    assert(JSON.parse(JSON.stringify(false)) == false)
    assert(JSON.parse(JSON.stringify(null)) == null)
    assert(JSON.parse(JSON.stringify(None)) == null)
  }

  test("parse: number") {
    assert(JSON.parse(JSON.stringify(123)) == 123)
    assert(JSON.parse(JSON.stringify(0.123)) == 0.123)
    assert(JSON.parse("0E-8") == 0) // big decimal
    assert(JSON.parse("1.234E0") == 1.234)
    assert(JSON.parse(JSON.stringify(0)) == 0)
  }

  test("parse: string") {
    assert(JSON.parse(JSON.stringify("")) == "")
    assert(JSON.parse(JSON.stringify("hello, world")) == "hello, world")
    assert(JSON.parse(JSON.stringify("a\"b")) == "a\"b")
    assert(JSON.parse(JSON.stringify("a\nb")) == "a\nb")
    assert(JSON.parse(JSON.stringify("a\"\nb")) == "a\"\nb")
  }

  test("parse: map") {
    assert(JSON.parse(JSON.stringify(Map())) == Map())
    assert(JSON.parse(JSON.stringify(Map("a" -> 1))) == Map("a" -> 1))
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
