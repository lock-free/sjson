package io.github.free.lock.sjson

import scala.collection.mutable.ListBuffer

class TokenParseTest extends org.scalatest.FunSuite {
  private def testToToken(txt: String, expect: List[JSONToken]) = {
    assert(TokenParser.toTokens(txt) == expect)

    // test async iterator mode
    val iter = new AsyncIterator[Char]()
    iter.pushList(txt.toList)

    val tokenIter = TokenParser.toTokenAsyncIterator(iter)

    val tokens = ListBuffer[JSONToken]()

    tokenIter.forEach((token, i) => {
      tokens.append(token)
    })

    iter.end()
    assert(tokens == expect)
  }

  test("toTokens: single number") {
    List("1.23e1", "1234", "0", "-0", "-123", "0.23", "-0.23", "1.23", "1.23e-1", "1.23e-123").map((txt) => {
      testToToken(txt, List(JSONToken(JSONToken.NUMBER, txt)))
    })
  }

  test("toTokens: single string") {
    List(s"""""""", s""""hello, world"""", s""""123"""", s""""\\""""",
      s""""\\n"""", s""""\t"""", s""""\\\\"""", s""""\\r"""", s""""\\b"""",
      s""""\\f"""", s""""\\/"""").map((txt) => {
      testToToken(txt, List(JSONToken(JSONToken.STRING, txt)))
    })
  }

  test("toTokens: true,false,null") {
    testToToken("true", List(JSONToken(JSONToken.TRUE, "true")))
    testToToken("false", List(JSONToken(JSONToken.FALSE, "false")))
    testToToken("null", List(JSONToken(JSONToken.NULL, "null")))
  }

  test("toTokens: array") {
    testToToken("[]", List(JSONToken(JSONToken.LEFT_PARAN, "[", 0), JSONToken(JSONToken.RIGHT_PARAN, "]", 1)))
    testToToken("[1234]", List(JSONToken(JSONToken.LEFT_PARAN, "[", 0), JSONToken(JSONToken.NUMBER, "1234", 1), JSONToken(JSONToken.RIGHT_PARAN, "]", 5)))
    testToToken("[1234, true]", List(JSONToken(JSONToken.LEFT_PARAN, "[", 0),
      JSONToken(JSONToken.NUMBER, "1234", 1),
      JSONToken(JSONToken.COMMA, ",", 5),
      JSONToken(JSONToken.TRUE, "true", 7),
      JSONToken(JSONToken.RIGHT_PARAN, "]", 11)))
  }

  test("toTokens: object") {
    testToToken("{}", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 1)))
    testToToken(s"""{"a": 1}""", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), 
      JSONToken(JSONToken.STRING, "\"a\"", 1),
      JSONToken(JSONToken.COLON, ":", 4),
      JSONToken(JSONToken.NUMBER, "1", 6),
      JSONToken(JSONToken.RIGHT_BRACKET, "}", 7)))
    testToToken(s"""{"a": 1, "b": null}""", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), 
      JSONToken(JSONToken.STRING, "\"a\"", 1),
      JSONToken(JSONToken.COLON, ":", 4),
      JSONToken(JSONToken.NUMBER, "1", 6),
      JSONToken(JSONToken.COMMA, ",", 7),
      JSONToken(JSONToken.STRING, "\"b\"", 9),
      JSONToken(JSONToken.COLON, ":", 12),
      JSONToken(JSONToken.NULL, "null", 14),
      JSONToken(JSONToken.RIGHT_BRACKET, "}", 18)))
  }

  test("toTokens: whitespace") {
    testToToken("{ }", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 2)))
    testToToken("{ \f}", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 3)))
    testToToken(s"""{
    }""", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 6)))
    testToToken(s"""{        }""", List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 9)))
  }
}
