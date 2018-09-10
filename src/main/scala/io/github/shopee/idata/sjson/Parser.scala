package io.github.shopee.idata.sjson

import scala.collection.mutable.ListBuffer

case class MidResolve(data: Any, lastToken: JSONToken, stack: ListBuffer[Any])

object JSONParser {
  def parse(jsonTxt: String) =
    parseHelp(jsonTxt)

  def toParseAsyncIterator(textIter: AsyncIterator[Char]): AsyncIterator[MidResolve] = {
    val result = new AsyncIterator[MidResolve]()
    var stack = ListBuffer[Any]()
    val tokenIter = TokenParser.toTokenAsyncIterator(textIter)

    tokenIter.forEach(itemHandler = (token, index) => {
      handleToken(token, stack, (mid) => {
        result.push(mid)
      })
    }, resultCallback = ResultCallback[Null](
        endCallback = (prev) => {
          if (stack.length == 0) {
            throw new Exception(s"Empty string.")
          }
          if (stack.length > 1) {
            throw new Exception(s"Unexpected token at last. Last stack=${stack}.")
          }
          result.end()
          prev
        },
        errorCallback = (err) => result.error(err)
      ))
    result
  }

  private def parseHelp(jsonTxt: String) = {
    val tokens = TokenParser.toTokens(jsonTxt)
    parseTokens(tokens)
  }

  // TODO error handle
  def parseTokens(tokens: List[JSONToken]) = {
    var stack = ListBuffer[Any]()
    tokens.foreach((token) => handleToken(token, stack))
    if (stack.length == 0) {
      throw new Exception(s"Empty string. Tokens=${tokens}.")
    }
    if (stack.length > 1) {
      throw new Exception(s"Unexpected token at last. Last stack=${stack}. Tokens=${tokens}.")
    }
    stack(0)
  }

  private def handleToken(token: JSONToken,
                          stack: ListBuffer[Any],
                          onResolve: (MidResolve) => Unit = null) = {
    val tokenText = token.text

    // 1. resolve atom value token
    val current = token.tokenType match {
      // basic value types
      case JSONToken.NUMBER => number(tokenText)
      case JSONToken.STRING => text(tokenText)
      case JSONToken.TRUE   => trueValue(tokenText)
      case JSONToken.FALSE  => falseValue(tokenText)
      case JSONToken.NULL   => nullValue(tokenText)
      case _                => token
    }

    if (isToken(current)) { // json token
      val curToken = current.asInstanceOf[JSONToken]

      if (isTokenType(current, JSONToken.RIGHT_BRACKET)) { // '}'
        if (stack.length == 0) {
          throw new Exception(errorAtToken(curToken, s"Unexpected token '${curToken.text}'"))
        }
        var objCnt = ListBuffer[Any]()

        // pop until find '{'
        while (!isTokenType(stack.last, JSONToken.LEFT_BRACKET)) {
          objCnt.append(stack.last)
          if (stack.length == 0) {
            throw new Exception(errorAtToken(curToken, s"missing '{'"))
          }
          stack.remove(stack.length - 1) // pop
        }
        stack.remove(stack.length - 1) // pop '{'

        // resolved an object value
        val objVal = objectValue(objCnt)
        if (onResolve != null) onResolve(MidResolve(objVal, token, stack))

        reduceValue(stack, objVal)
      } else if (isTokenType(current, JSONToken.RIGHT_PARAN)) { // ']'
        if (stack.length == 0) {
          throw new Exception(errorAtToken(curToken, s"Unexpected token '${curToken.text}'"))
        }
        var arrCnt = ListBuffer[Any]()

        // pop until find '['
        while (!isTokenType(stack.last, JSONToken.LEFT_PARAN)) {
          arrCnt.append(stack.last)
          if (stack.length == 0) {
            throw new Exception(errorAtToken(curToken, s"missing '['"))
          }
          stack.remove(stack.length - 1) // pop
        }
        stack.remove(stack.length - 1) // pop '['

        // resolved an array
        val arrVal = arrayValue(arrCnt)
        if (onResolve != null) onResolve(MidResolve(arrVal, token, stack))

        reduceValue(stack, arrVal)
      } else {
        // other atom values
        if (onResolve != null) onResolve(MidResolve(current, token, stack))

        stack.append(current) // push
      }
    } else { // value
      reduceValue(stack, current)
    }
  }

  private def reduceValue(stack: ListBuffer[Any], value: Any) {

    /**
      * When find a ':' at the top of stack, try to pair it with previous key, `key: value`
      */
    if (stack.length > 0 && isTokenType(stack.last, JSONToken.COLON)) { // :
      val top = stack.last.asInstanceOf[JSONToken]
      stack.remove(stack.length - 1) // pop :
      if (stack.length == 0) {
        throw new Exception(errorAtToken(top, s"Unexpected token '${top.text}'"))
      }

      val key = stack.last
      stack.remove(stack.length - 1)

      if (!key.isInstanceOf[String]) {
        throw new Exception(
          errorAtToken(top, s"In a pair, key should be a string, but got $key")
        )
      }

      // find a pair
      stack.append(pair(key.asInstanceOf[String], value)) // push pair
    } else { // do nothing, just push
      stack.append(value) // push
    }
    stack
  }

  // value to convert
  // TODO
  private def number(txt: String): Any =
    if (txt.contains(".")) {
      txt.toDouble
    } else {
      val value = BigDecimal(new java.math.BigDecimal(txt))
      if (value >= -2147483648 && value <= 2147483647) {
        value.toInt
      } else {
        value
      }
    }

  private def text(txt: String): String        = JSONUtil.unescapeString(txt)
  private def trueValue(txt: String): Boolean  = true
  private def falseValue(txt: String): Boolean = false
  private def nullValue(txt: String)           = null

  case class Pair(key: String, value: Any)
  case class Elements(list: List[Any])
  private def pair(key: String, value: Any): Pair = Pair(key, value)

  private def objectValue(objCnt: ListBuffer[Any]): Map[String, Any] = {
    var tupleList = List[(String, Any)]()

    var i = 0
    while (i < objCnt.length) {
      val p = objCnt(i)
      p match {
        case Pair(key, value) => {
          val pairP = p.asInstanceOf[Pair]
          tupleList = (pairP.key, pairP.value) :: tupleList
        }
        case _ => {
          throw new Exception("object can only composed by pairs.")
        }
      }

      i += 1

      if (i == objCnt.length - 1) {
        throw new Exception("Unexpected token")
      }

      if (i < objCnt.length) {
        val comma = expectToken(objCnt(i), JSONToken.COMMA)
        i += 1
      }
    }

    tupleList.toMap
  }

  private def arrayValue(arrCnt: ListBuffer[Any]): List[Any] = {
    var i    = 0
    var list = List[Any]()

    while (i < arrCnt.length) {
      val v = arrCnt(i)
      v match {
        case Pair(key, value) => {
          throw new Exception("array can not contain pair.")
        }
        case _ => {
          list = v :: list
        }
      }

      i += 1

      // check comma
      if (i == arrCnt.length - 1) {
        throw new Exception("Unexpected token")
      }

      if (i < arrCnt.length) {
        val comma = expectToken(arrCnt(i), JSONToken.COMMA)
        i += 1
      }
    }

    list
  }

  private def isToken(v: Any): Boolean = v.isInstanceOf[JSONToken]
  private def isTokenType(v: Any, tokenType: Int): Boolean =
    isToken(v) && v.asInstanceOf[JSONToken].tokenType == tokenType
  private def expectToken(v: Any, tokenType: Int): JSONToken = {
    if (!isToken(v)) {
      throw new Exception(s"Expect token here.")
    }

    val token = v.asInstanceOf[JSONToken]
    if (token.tokenType != tokenType) {
      throw new Exception(
        s"Expect token with type ${tokenType}, but got token with type ${token.tokenType}."
      )
    }

    token
  }

  private def errorAtToken(token: JSONToken, errMsg: String): String =
    s"""[${token.startIndex}]${errMsg}. Error happend nearby '${token.text}'(${token.tokenType})"""
}
