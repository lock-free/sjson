package io.github.shopee.idata.sjson

class TokenParseTest extends org.scalatest.FunSuite {
  test("toTokens: single number") {
    assert(TokenParser.toTokens("1234") == List(JSONToken(JSONToken.NUMBER, "1234")))
    assert(TokenParser.toTokens("0") == List(JSONToken(JSONToken.NUMBER, "0")))
    assert(TokenParser.toTokens("0.23") == List(JSONToken(JSONToken.NUMBER, "0.23")))
    assert(TokenParser.toTokens("1.23") == List(JSONToken(JSONToken.NUMBER, "1.23")))
    assert(TokenParser.toTokens("1.23e1") == List(JSONToken(JSONToken.NUMBER, "1.23e1")))
    assert(TokenParser.toTokens("1.23e-1") == List(JSONToken(JSONToken.NUMBER, "1.23e-1")))
  }

  test("toTokens: single string") {
    assert(TokenParser.toTokens(s"""""""") == List(JSONToken(JSONToken.STRING, s"""""""")))
    assert(TokenParser.toTokens(s""""hello, world"""") == List(JSONToken(JSONToken.STRING, s""""hello, world"""")))
    assert(TokenParser.toTokens(s""""123"""") == List(JSONToken(JSONToken.STRING, s""""123"""")))
    assert(TokenParser.toTokens(s""""\\""""") == List(JSONToken(JSONToken.STRING, s""""\\""""")))
    assert(TokenParser.toTokens(s""""\n"""") == List(JSONToken(JSONToken.STRING, s""""\n"""")))
    assert(TokenParser.toTokens(s""""\t"""") == List(JSONToken(JSONToken.STRING, s""""\t"""")))
    assert(TokenParser.toTokens(s""""\\\\"""") == List(JSONToken(JSONToken.STRING, s""""\\\\"""")))
  }

  test("toTokens: true,false,null") {
    assert(TokenParser.toTokens("true") == List(JSONToken(JSONToken.TRUE, "true")))
    assert(TokenParser.toTokens("false") == List(JSONToken(JSONToken.FALSE, "false")))
    assert(TokenParser.toTokens("null") == List(JSONToken(JSONToken.NULL, "null")))
  }

  test("toTokens: array") {
    assert(TokenParser.toTokens("[]") == List(JSONToken(JSONToken.LEFT_PARAN, "[", 0), JSONToken(JSONToken.RIGHT_PARAN, "]", 1)))
    assert(TokenParser.toTokens("[1234]") == List(JSONToken(JSONToken.LEFT_PARAN, "[", 0), JSONToken(JSONToken.NUMBER, "1234", 1), JSONToken(JSONToken.RIGHT_PARAN, "]", 5)))
    assert(TokenParser.toTokens("[1234, true]") == List(JSONToken(JSONToken.LEFT_PARAN, "[", 0),
      JSONToken(JSONToken.NUMBER, "1234", 1),
      JSONToken(JSONToken.COMMA, ",", 5),
      JSONToken(JSONToken.TRUE, "true", 7),
      JSONToken(JSONToken.RIGHT_PARAN, "]", 11)))
  }

  test("toTokens: object") {
    assert(TokenParser.toTokens("{}") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 1)))
    assert(TokenParser.toTokens(s"""{"a": 1}""") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), 
      JSONToken(JSONToken.STRING, "\"a\"", 1),
      JSONToken(JSONToken.COLON, ":", 4),
      JSONToken(JSONToken.NUMBER, "1", 6),
      JSONToken(JSONToken.RIGHT_BRACKET, "}", 7)))
    assert(TokenParser.toTokens(s"""{"a": 1, "b": null}""") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), 
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
    assert(TokenParser.toTokens("{ }") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 2)))
    assert(TokenParser.toTokens("{ \f}") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 3)))
    assert(TokenParser.toTokens(s"""{
    }""") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 6)))
assert(TokenParser.toTokens(s"""{        }""") == List(JSONToken(JSONToken.LEFT_BRACKET, "{", 0), JSONToken(JSONToken.RIGHT_BRACKET, "}", 9)))
  }
}
