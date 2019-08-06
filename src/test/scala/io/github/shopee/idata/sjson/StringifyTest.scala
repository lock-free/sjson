package io.github.free.lock.sjson

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
    assert(JSON.stringify(Double.NaN) == "null")
    assert(JSON.stringify(Float.NaN) == "null")
    assert(JSON.stringify(1.0 / 0.0) == "null")
  }

  test("stringify: boolean") {
    assert(JSON.stringify(true) == "true")
    assert(JSON.stringify(false) == "false")
  }

  test("stringify: null and none") {
    assert(JSON.stringify(null) == "null")
    assert(JSON.stringify(()) == "null")
    assert(JSON.stringify(None) == "null")
  }

  test("stringify: Timestamp") {
    assert(
      JSON.stringify(new java.sql.Timestamp(1990 - 1900, 2, 12, 0, 0, 0, 0)) == "\"1990-03-12 00:00:00.0\""
    )
  }

  test("stringify: java.sql.time") {
    assert(JSON.stringify(new java.sql.Time(11, 20, 3)) == "\"11:20:03\"")
  }

  test("stringify: java.sql.Date") {
    assert(JSON.stringify(new java.sql.Date(1990 - 1900, 2, 12)) == "\"1990-03-12\"")
  }

  test("stringify: java.util.Date") {
    assert(
      JSON.stringify(new java.util.Date(1990 - 1900, 2, 12)) == s""""${new java.util.Date(
        1990 - 1900,
        2,
        12
      ).toString()}""""
    )
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

  test("replacer") {
    case class User(name: String, age: Int, login: java.util.Date)
    val user = User("ddchen", 10, new java.util.Date(1990 - 1900, 2, 12))
    assert(
      JSON.stringify(
        user,
        (data, path) => {
          if (path.mkString(".") == "login") {
            val date = data.asInstanceOf[java.util.Date]
            Some(s""""${date.getYear() + 1900},${date.getMonth() + 1},${date.getDate()}"""")
          } else {
            None
          }
        }
      ) === s"""{"name":"ddchen","age":10,"login":"1990,3,12"}"""
    )
  }

  test("replacer2") {
    case class User(name: String, age: Int, firends: Map[String, User] = Map[String, User]())
    val user = User("ddchen", 10, Map("a" -> User("a", 100)))
    assert(
      JSON.stringify(
        user,
        (data, path) => {
          if (path.reverse.mkString(".") == "friends.a") {
            Some("a")
          } else {
            None
          }
        }
      ) === s"""{"name":"ddchen","age":10,"firends":{"a":{"name":"a","age":100,"firends":{}}}}"""
    )
  }

  test("circle: base") {
    val v1 = new Array[Any](3)
    v1(0) = 1
    v1(1) = 2
    v1(2) = v1
    try {
      JSON.stringify(v1)
      assert(false)
    } catch {
      case e: Exception => {
        assert(e.getMessage.indexOf("circle detected") != -1)
      }
    }
  }

  test("circle2: deeper") {
    val v1 = new Array[Any](2)
    val v2 = Map(
      "a" -> v1
    )

    v1(0) = 1
    v1(1) = v2
    try {
      JSON.stringify(v1)
      assert(false)
    } catch {
      case e: Exception => {
        assert(e.getMessage.indexOf("circle detected") != -1)
      }
    }
  }
}
