package io.github.shopee.idata.sjson

class stringifyTest extends org.scalatest.FunSuite {
  test("stringify: string1") {
    assert(JSON.stringify("123") == s""""123"""")
  }

  test("stringify: string2") {
    assert(JSON.stringify(s"""123
4""") == s""""123\\n4"""")
  }

  test("stringify: number") {
    assert(JSON.stringify(123) == "123")
    assert(JSON.stringify(1.toShort) == "1")
    assert(JSON.stringify(3.1f) == "3.1")
    val d: Double = 234.134
    assert(JSON.stringify(d) == "234.134")
    assert(JSON.stringify(123456l) == "123456")
  }

  test("stringify: boolean") {
    assert(JSON.stringify(true) == "true")
    assert(JSON.stringify(false) == "false")
  }

  test("stringify: null and none") {
    assert(JSON.stringify(null) == "null")
    assert(JSON.stringify(None) == "null")
  }

  test("stringify: map") {
    assert(JSON.stringify(scala.collection.mutable.Map[String, Any]()) == "{}")
    assert(JSON.stringify(scala.collection.immutable.Map[String, Any]()) == "{}")
    assert(JSON.stringify(scala.collection.mutable.Map("1"   -> 2)) == "{\"1\":2}")
    assert(JSON.stringify(scala.collection.immutable.Map("1" -> 2)) == "{\"1\":2}")
    assert(
      JSON.stringify(scala.collection.mutable.Map("a" -> 1, "b" -> "2")) == "{\"b\":\"2\",\"a\":1}"
    )
    assert(
      JSON
        .stringify(scala.collection.immutable.Map("a" -> 1, "b" -> "2")) == "{\"a\":1,\"b\":\"2\"}"
    )

    assert(
      JSON.stringify(
        Map(
          "userid"     -> 77376269,
          "username"   -> "farreal1",
          "last_login" -> "2018-07-08 20:22:17.0",
          "birthday"   -> "1970-01-01"
        )
      ) == "{\"userid\":77376269,\"username\":\"farreal1\",\"last_login\":\"2018-07-08 20:22:17.0\",\"birthday\":\"1970-01-01\"}"
    )
  }

  test("stringify: list") {
    assert(JSON.stringify(List()) == "[]")
    assert(JSON.stringify(Vector()) == "[]")
    assert(JSON.stringify(Array()) == "[]")
    assert(JSON.stringify(List(1)) == "[1]")
    assert(JSON.stringify(List(1, "2")) == "[1,\"2\"]")

    assert(
      JSON.stringify(
        List(
          Map(
            "userid"     -> 77376269,
            "username"   -> "farreal1",
            "last_login" -> "2018-07-08 20:22:17.0",
            "birthday"   -> "1970-01-01"
          )
        )
      ) == "[{\"userid\":77376269,\"username\":\"farreal1\",\"last_login\":\"2018-07-08 20:22:17.0\",\"birthday\":\"1970-01-01\"}]"
    )
  }

  test("stringify: nested normal") {
    assert(
      JSON.stringify(List(Map[String, Any]("a" -> true), 1, 2, null)) == """[{"a":true},1,2,null]"""
    )
  }

  test("case class: base") {
    case class User(name: String, age: Int)
    val user = User("ddchen", 10)
    assert(JSON.stringify(user) == """{"name":"ddchen","age":10}""")
  }

  test("case class: nested") {
    case class User(name: String, age: Int)
    case class Group(groupName: String, users: List[User])
    val user1 = User("ddchen", 10)
    val user2 = User("ddchen2", 12)
    val user3 = User("ddchen3", 12)
    val group = Group("test", List(user1, user2, user3))

    assert(
      JSON.stringify(group) == """{"groupName":"test","users":[{"name":"ddchen","age":10},{"name":"ddchen2","age":12},{"name":"ddchen3","age":12}]}"""
    )
  }

}
