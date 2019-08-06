package io.github.free.lock.json.example

import io.github.free.lock.sjson.JSON

case class User(name: String, age: Int)

object Main {
  def main(args: Array[String]) {
    println(JSON.stringify(List(1,2,3)))
		println(JSON.stringify(Map(
      "items" -> List(1,2,3),
      "type" -> "test"
		))) // {"items": [1,2,3], "type": "testl"}
		
		println(JSON.stringify(User("NoName", 7))) // {"name": "NoName", age: 7}
		
		// parse
		println(JSON.parse("[1,2,3]")) // List(1,2,3)
		println(JSON.parse(s"""{"a":1,"b":2}""")) // Map("a" -> 1, "b" -> 2)
  }
}
