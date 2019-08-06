package io.github.free.lock.sjson

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
}
