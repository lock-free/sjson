# sjson

JSON library for Scala

JSON specification:
http://json.org/
https://www.ietf.org/rfc/rfc4627.txt

## features

- Automatic type converting

- support customization converting

- Streaming support (TODO)

## quick example

```
import io.github.shopee.idata.sjson.JSON

JSON.stringify(List(1,2,3)) // [1,2,3]
JSON.stringify(Map(
  "items": List(1,2,3),
  "type": "test"
)) // {"items": [1,2,3], "type": "testl"}

case class User(name: String, age: Int)
JSON.stringify(User("NoName", 7)) // {"name": "NoName", age: 7}
```
