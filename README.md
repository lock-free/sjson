# sjson

JSON library for Scala

![travis-ci build status](https://travis-ci.com/lock-free/sjson.svg?branch=master)

> Converting JSON and Scala automaticly

<!-- toc -->

- [JSON specification](#json-specification)
- [Features](#features)
- [Quick example](#quick-example)
- [Install](#install)
- [Main Apis](#main-apis)
  * [JSON.stringify](#jsonstringify)
  * [JSON.parse](#jsonparse)
  * [JSON.convert](#jsonconvert)
  * [JSON.parseTo](#jsonparseto)
- [Streaming example](#streaming-example)

<!-- tocstop -->

## JSON specification

- http://json.org/
- https://www.ietf.org/rfc/rfc4627.txt

## Features

- Type converting automaticly

- Support customization converting

- Streaming support

## Quick example

```scala
package io.github.free.lock.json.example

import io.github.free.lock.sjson.JSON

case class User(name: String, age: Int)

object Main {
  def main(args: Array[String]) {
    // [1,2,3]
    JSON.stringify(List(1,2,3))

    // {"items": [1,2,3], "type": "testl"}
    JSON.stringify(Map("items" -> List(1,2,3), "type" -> "test"))
		
    // {"name": "NoName", age: 7}
    JSON.stringify(User("NoName", 7))
		
		/* parse to plain scala object */
    // List(1,2,3)
    JSON.parse("[1,2,3]") 

    // Map("a" -> 1, "b" -> 2)
    JSON.parse(s"""{"a":1,"b":2}""") 

    // parse to target type
    // User("ddchen", 10)
    JSON.parseTo[User](s"""{"name":"ddchen","age":10}""")
  }
}
```

## Install

- install by sbt

```
libraryDependencies ++= Seq(
  "io.github.lock-free" %% "sjson" % "0.1.2"
)
```

## Main Apis

### JSON.stringify

`JSON.stringify(value: Any[, replacer]): String`

Convert a scala object to json text.

- Default converting rules

At most times, you just need to follow the basic rules and do not need to write any converters by yourself. SJSON would use following basic rules, stringify value recursively.

scala type | json type
--- | ---
String | string
Char | string
java.sql.Timestamp | string
java.util.Date | string
Number | number
Boolean | bool
None | null
Null | null
Unit | null
NaN  | null
Infinity  | null
AbstractSeq[_] | array
scala.collection.mutable.AbstractSeq[_] | array
Array[_] | array
scala.collection.mutable.Map[_, _\] | object
scala.collection.immutable.Map[_, _\] | object 
other class | object (field name -> field value)

```scala
import io.github.free.lock.sjson.JSON

// case class
case class User(name: String, age: Int)

val user = User("ddchen", 10)

JSON.stringify(user) // {"name":"ddchen","age":10}

JSON.stringify(List(1, 2, 3)) // [1,2,3]

JSON.stringify(Map(
  "user" -> user,
  "data" -> List(1,2,3)
)) // {"user":{"name":"ddchen","age":10},"data":[1,2,3]}
```

- Customize replacer

If you want to change the process of stringify, you can use optional paramater `replacer`.

`replacer:  (data, path) -> Option[newData]`, data is the value you are stringifying currently and path is the reversed json path.

```scala
case class User(name: String, age: Int, login: java.util.Date)

val user = User("ddchen", 10, new java.util.Date(1990 - 1900, 2, 12))

// {"name":"ddchen","age":10,"login":"1990,3,12"}
JSON.stringify(user, (data, path) => {

  // path stored json paths in a reversed way. (path is a stack)
  if (path.reserse.mkString(".") == "login") {

    val date = data.asInstanceOf[java.util.Date]
    Some(s""""${date.getYear() + 1900},${date.getMonth() + 1},${date.getDate()}"""") // new stringify result for data
  } else {

    // None means do not change the origin converting rule
    None
  }
})
```

### JSON.parse

`JSON.parse(jsonTxt: String[, replacer]): Any`

Convert a json text to a scala object

json type | scala type
--- | ---
string | String
number | Number
bool | Boolean
null | Null
array | List[Any]
object | Map[String, Any]

After parsing, you will get a plain scala object. Then you can use `JSON.convert[T](obj)` to convert this object to target scala type.

### JSON.convert

`JSON.convert[T](value: Any): T`

- Default converting rules

plain scala type | scala type
--- | ---
same type | same type 
List | Array
List | Vector
Map | Map
Map | Case class
Nothing | Null

### JSON.parseTo

`JSON.parseTo[T](txt) = JSON.convert[T](JSON.parse(txt))`

## Streaming example

```scala
import io.github.free.lock.sjson.{JSON, AsyncIterator}

val textIter = new AsyncIterator[Char]()

// stream handle part
val parseIter = JSON.parseAsyncIterator(textIter, (data, pathStack, _) => {
    if (JSON.toJsonPath(pathStack) == "data.[0]") { // because we wiped this data, so the json path should always be "data.[0]"
      // handle data here
      print(data) // can caputure all data like {"value":"datak"} in this example

      JSON.WIPE_VALUE // do not store data in memory
    } else data
  }
)

// another place to accept streaming data
textIter.pushList(s"""{"type": "normal", """) // 1st chunk
// data part
textIter.pushList(s""""data": [""") // 2th chunk
textIter.pushList(s"""{"value":"data1"}, {"value":"data2"}, """)
textIter.pushList(s"""{"value":"data3"}, {"value":"data4"}, """)
textIter.pushList(s"""{"value":"data5"}, {"value":"data6"}, """)
// ...
textIter.pushList(s"""{"value":"datan"}""")
textIter.pushList("]}") // last chunk
```
