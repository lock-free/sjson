package com.shopee.idata.sjson

import scala.collection.mutable.ListBuffer

case class JSONToken(tokenType: Int, text: String, startIndex: Int = 0)

object JSONToken {
  val STRING = 0
  val NUMBER = 1

  val TRUE  = 2
  val FALSE = 3
  val NULL  = 4

  val LEFT_PARAN  = 5 // {
  val RIGHT_PARAN = 6 // }

  val LEFT_BRACKET  = 7 // [
  val RIGHT_BRACKET = 8 // ]

  val COMMA = 9  // ,
  val COLON = 10 // :
}

object TokenParser {
  def toTokens(jsonTxt: String): List[JSONToken] = {
    val tokensBuilder = ListBuffer[JSONToken]()
    val jsonTxtLen    = jsonTxt.length;

    var i = 0
    while (i < jsonTxtLen) {
      val ch = jsonTxt(i)
      val token = ch match {
        case '{'  => JSONToken(JSONToken.LEFT_BRACKET, ch + "", i)
        case '}'  => JSONToken(JSONToken.RIGHT_BRACKET, ch + "", i)
        case '['  => JSONToken(JSONToken.LEFT_PARAN, ch + "", i)
        case ']'  => JSONToken(JSONToken.RIGHT_PARAN, ch + "", i)
        case ','  => JSONToken(JSONToken.COMMA, ch + "", i)
        case ':'  => JSONToken(JSONToken.COLON, ch + "", i)
        case ' '  => null // white space
        case '\t' => null // white space
        case '\n' => null // white space
        case '\f' => null // white space
        case '"' => { // string
          val txtBuilder = new StringBuilder // use txt builder to collect text
          txtBuilder.append("\"")
          var j      = i + 1
          var closed = false

          // try to find another " with out escaped
          while (j < jsonTxtLen && !closed) {
            // TODO make this stronger
            // http://json.org
            if (jsonTxt(j) == '\\') {
              if (j + 2 > jsonTxtLen) {
                throw new Exception(tokenParseError(jsonTxt, j, "text should not end up with \\."))
              }
              txtBuilder.append(jsonTxt(j))
              txtBuilder.append(jsonTxt(j + 1))
              j += 2
            } else if (jsonTxt(j) == '"') {
              closed = true
              txtBuilder.append('"')
              j += 1
            } else {
              txtBuilder.append(jsonTxt(j))
              j += 1
            }
          }

          if (!closed) {
            throw new Exception(
              tokenParseError(jsonTxt, j, """missing '"' to close string text.""")
            )
          }

          JSONToken(JSONToken.STRING, txtBuilder.toString(), i)
        }

        case _ => {
          if (ch == '-' || (ch >= '0' && ch <= '9')) {
            var j = i

            val numberBuilder = new StringBuilder
            // negative symbol, -
            if (ch == '-') {
              numberBuilder.append(ch)
              j += 1
            }

            // integer part, 01234
            while (j < jsonTxtLen && jsonTxt(j) >= '0' && jsonTxt(j) <= '9') {
              numberBuilder.append(jsonTxt(j))
              j += 1
            }

            // fragment part, .01234
            if (j < jsonTxt.length && jsonTxt(j) == '.') {
              numberBuilder.append(jsonTxt(j))
              j += 1

              while (j < jsonTxtLen && jsonTxt(j) >= '0' && jsonTxt(j) <= '9') {
                numberBuilder.append(jsonTxt(j))
                j += 1
              }
            }

            // science part
            if (j < jsonTxtLen && (jsonTxt(j) == 'e' || jsonTxt(j) == 'E')) {
              numberBuilder.append(jsonTxt(j))
              j += 1

              if (j < jsonTxtLen && (jsonTxt(j) == '+' || jsonTxt(j) == '-')) {
                numberBuilder.append(jsonTxt(j))
                j += 1
              }

              while (j < jsonTxtLen && (jsonTxt(j) >= '0' && jsonTxt(j) <= '9')) {
                numberBuilder.append(jsonTxt(j))
                j += 1
              }
            }

            val numberText = numberBuilder.toString()
            if (numberText == "-") {
              throw new Exception(tokenParseError(jsonTxt, j, "expect number after '-'"))
            }

            JSONToken(JSONToken.NUMBER, numberText, i)
          } else {
            if (i + 3 < jsonTxtLen && jsonTxt.substring(i, i + 4) == "true") {
              JSONToken(JSONToken.TRUE, "true", i)
            } else if (i + 4 < jsonTxtLen && jsonTxt.substring(i, i + 5) == "false") {
              JSONToken(JSONToken.FALSE, "false", i)
            } else if (i + 3 < jsonTxtLen && jsonTxt.substring(i, i + 4) == "null") {
              JSONToken(JSONToken.NULL, "null", i)
            } else {
              throw new Exception(
                tokenParseError(
                  jsonTxt,
                  i,
                  s"unrecorgnized symbol '$ch', the int value of char is ${ch.toInt}."
                )
              )
            }
          }
        }
      }

      if (token != null) {
        tokensBuilder.append(token)
        i += token.text.length
      } else { // ignore
        i += 1
      }
    }

    tokensBuilder.toList
  }

  private def tokenParseError(txt: String, location: Int, errorMessage: String): String = {
    val prev       = if (location - 5 <= 0) 0 else location - 5
    val after      = if (location + 5 >= txt.length) txt.length - 1 else location + 5
    val prevNearBy = if (prev > location) "" else txt.substring(prev, location)
    val afterNearBy =
      if (location + 1 > after) "" else txt.substring(location + 1, after)
    val cur = if (location < txt.length) txt(location) else ""
    s"""[${location}]${errorMessage}. Error happened nearby '${prevNearBy} >${cur}< ${afterNearBy}'."""
  }
}
