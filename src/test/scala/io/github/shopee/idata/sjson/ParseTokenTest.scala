package io.github.free.lock.sjson

class ParseTokenTest extends org.scalatest.FunSuite {
  test("parseTokens") {
    assert(JSONParser.parseTokens(List(JSONToken(JSONToken.NUMBER, "123"))) == 123)
    assert(JSONParser.parseTokens(List(JSONToken(JSONToken.STRING, """"hello""""))) == "hello")
    assert(JSONParser.parseTokens(List(JSONToken(JSONToken.TRUE, "true"))) == true)
    assert(
      JSONParser
        .parseTokens(List(JSONToken(JSONToken.FALSE, "false"))) == false
    )
    assert(JSONParser.parseTokens(List(JSONToken(JSONToken.NULL, "null"))) == null)
  }

  test("parseTokens: object") {
    assert(
      JSONParser.parseTokens(
        List(JSONToken(JSONToken.LEFT_BRACKET, "{"), JSONToken(JSONToken.RIGHT_BRACKET, "}"))
      ) == Map()
    )

    // {"a": 123}
    assert(
      JSONParser.parseTokens(
        List(
          JSONToken(JSONToken.LEFT_BRACKET, "{"),
          JSONToken(JSONToken.STRING, "\"a\""),
          JSONToken(JSONToken.COLON, ":"),
          JSONToken(JSONToken.NUMBER, "123"),
          JSONToken(JSONToken.RIGHT_BRACKET, "}")
        )
      ) == Map("a" -> 123.0)
    )

    // {"a": 123, "b": true}
    assert(
      JSONParser.parseTokens(
        List(
          JSONToken(JSONToken.LEFT_BRACKET, "{"),
          JSONToken(JSONToken.STRING, "\"a\""),
          JSONToken(JSONToken.COLON, ":"),
          JSONToken(JSONToken.NUMBER, "123"),
          JSONToken(JSONToken.COMMA, ","),
          JSONToken(JSONToken.STRING, "\"b\""),
          JSONToken(JSONToken.COLON, ":"),
          JSONToken(JSONToken.TRUE, "true"),
          JSONToken(JSONToken.RIGHT_BRACKET, "}")
        )
      ) == Map("a" -> 123.0, "b" -> true)
    )
  }

  test("parseTokens: array") {
    assert(
      JSONParser.parseTokens(
        List(JSONToken(JSONToken.LEFT_PARAN, "["), JSONToken(JSONToken.RIGHT_PARAN, "]"))
      ) == List()
    )

    // [123]
    assert(
      JSONParser.parseTokens(
        List(
          JSONToken(JSONToken.LEFT_PARAN, "["),
          JSONToken(JSONToken.NUMBER, "123"),
          JSONToken(JSONToken.RIGHT_PARAN, "]")
        )
      ) == List(123.0)
    )

    // [123, true]
    assert(
      JSONParser.parseTokens(
        List(
          JSONToken(JSONToken.LEFT_PARAN, "["),
          JSONToken(JSONToken.NUMBER, "123"),
          JSONToken(JSONToken.COMMA, ","),
          JSONToken(JSONToken.TRUE, "true"),
          JSONToken(JSONToken.RIGHT_PARAN, "]")
        )
      ) == List(123.0, true)
    )
  }
}
