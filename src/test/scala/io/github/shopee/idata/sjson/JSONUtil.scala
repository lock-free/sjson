package io.github.free.lock.sjson

import org.scalatest.Matchers.{convertToAnyShouldWrapper, equal}

class JSONUtilTest extends org.scalatest.FunSuite {
  test("unescapeString") {
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("1234")) == "1234")
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("12\"34")) == "12\"34")
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("12\n34")) == "12\n34")
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("12\b34")) == "12\b34")
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("12\r34")) == "12\r34")
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("12\f34")) == "12\f34")
    assert(JSONUtil.unescapeString(JSONUtil.escapeString("12\\34")) == "12\\34")
  }

  test("unicodeArrayBuilderSimple"){
    val currString = "\"\\uD835\\uD83\""
    val unicodeArray = new Array[Boolean](currString.length)
    unicodeArray.update(1, true)
    JSONUtil.uniCodeArrayBuilder(currString) should equal (unicodeArray)
  }

  test("unicodeArrayBuilderWithInvalidUniCodeInBetween2"){
    val currString = "\"\\uD835\\uD835\\uD83\\uDC07\""
    val unicodeArray = new Array[Boolean](currString.length)
    unicodeArray.update(1, true)
    unicodeArray.update(7, true)
    unicodeArray.update(18, true)
    JSONUtil.uniCodeArrayBuilder(currString) should equal (unicodeArray)
  }

  test("unicodeArrayBuilderWithInvalidUniCodeInBetween"){
    val currString = "\"\\uD835\\uD83\\uD835\""
    val unicodeArray = new Array[Boolean](currString.length)
    unicodeArray.update(1, true)
    unicodeArray.update(12, true)
    JSONUtil.uniCodeArrayBuilder(currString) should equal (unicodeArray)
  }

  test("unicodeArrayNoUnicode"){
    val currString = "\"\\uD83za\\uD83zxd\""
    val unicodeArray = new Array[Boolean](currString.length)
    JSONUtil.uniCodeArrayBuilder(currString) should equal (unicodeArray)
  }
}
