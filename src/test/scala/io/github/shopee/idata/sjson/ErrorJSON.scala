package io.github.shopee.idata.sjson

class ErrorJSONTest extends org.scalatest.FunSuite {
  test("tokenParser: string end up with \\") {
    try {
      JSON.parse(s""""1234\\""")
      throw new Exception("should have error")
    } catch {
      case e: Exception => {
        assert(e.getMessage.indexOf("text should not end up with \\.") !== -1)
      }
    }
  }

  test("tokenParser: missing \"") {
    try {
      JSON.parse(s""""1234""")
      throw new Exception("should have error")
    } catch {
      case e: Exception => {
        assert(e.getMessage.indexOf("""missing '"' to close string text.""") !== -1)
      }
    }
  }

  test("tokenParser: - without number") {
    try {
      JSON.parse(s"""-""")
      throw new Exception("should have error")
    } catch {
      case e: Exception => {
        assert(e.getMessage.indexOf("expect number after '-'") !== -1)
      }
    }
  }

  test("tokenParse: unrecorgnized symbol") {
    try {
      JSON.parse(s"""1234k""")
      throw new Exception("should have error")
    } catch {
      case e: Exception => {
        assert(e.getMessage.indexOf("unrecorgnized symbol 'k'") !== -1)
      }
    }
  }

  test("tokenParse: unit type") {
    val fun = () => {}
    assert(JSON.stringify(fun()) == "null")
  }
}
