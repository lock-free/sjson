package io.github.free.lock.sjson

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

class SpecialParserValue()

object PathNode {
  val ARRAY_CTX = 0
  val OBJ_CTX   = 1
}

case class PathNode(ntype: Int, index: String)

// do not use stack.length, it's too slow
object JSONParser {
  val WIPE_VALUE = new SpecialParserValue()

  // replacer can middle the parsing process
  // data, lastToken, stack
  type ParseReplacer = (Any, Stack[PathNode], Stack[Any]) => Any
  def defParseReplacer(data: Any, path: Stack[PathNode], stack: Stack[Any]) = data

  def parse(jsonTxt: String, parseReplacer: ParseReplacer = defParseReplacer): Any = {
    val tokens = TokenParser.toTokens(jsonTxt)
    parseTokens(tokens, parseReplacer)
  }

  def parseAsyncIterator(textIter: AsyncIterator[Char],
                         parseReplacer: ParseReplacer = defParseReplacer): AsyncIterator[Any] = {
    val result    = new AsyncIterator[Any]()
    val stack     = Stack[Any]()
    val path      = Stack[PathNode]()
    val tokenIter = TokenParser.toTokenAsyncIterator(textIter)

    tokenIter.forEach(
      itemHandler = (token, index) => {
        handleToken(token, stack, path, (data, path, stack) => {
          val value = parseReplacer(data, path, stack)
          result.push(value)
          value
        })
      },
      resultCallback = ResultCallback[Null](
        endCallback = (prev) => {
          if (stack.isEmpty) {
            throw new Exception(s"Empty string.")
          }
          if (stack.length > 1) {
            throw new Exception(s"Unexpected token at last. Last stack=${stack}.")
          }
          result.end()
          prev
        },
        errorCallback = (err) => result.error(err)
      )
    )
    result
  }

  def toJsonPath(path: Stack[PathNode]): String = {
    val pathBuilder = new ListBuffer[String]()
    path.map((item) => {
      if (item.ntype == PathNode.ARRAY_CTX) {
        pathBuilder.append("[" + item.index + "]")
      } else {
        pathBuilder.append(item.index)
      }
    })
    pathBuilder.reverse.mkString(".")
  }

  def parseTokens(tokens: List[JSONToken], parseReplacer: ParseReplacer = defParseReplacer): Any = {
    var stack = Stack[Any]()
    var path  = Stack[PathNode]()
    tokens.foreach((token) => handleToken(token, stack, path, parseReplacer))

    if (stack.isEmpty) {
      throw new Exception(s"Empty string. Tokens=${tokens}.")
    }

    val top = stack.pop()

    if (!stack.isEmpty) {
      throw new Exception(s"Unexpected token at last. Last stack=${stack}. Tokens=${tokens}.")
    }

    top
  }

  private def handleToken(token: JSONToken,
                          stack: Stack[Any],
                          path: Stack[PathNode],
                          parseReplacer: ParseReplacer) =
    if (!cleanWipedData(stack, token)) {
      updatePath(path, stack, token)
      val tokenText = token.text

      reduceValue(
        stack,
        token.tokenType match {
          case JSONToken.NUMBER =>
            if (tokenText.contains(".")) {
              tokenText.toDouble
            } else {
              val value = BigDecimal(new java.math.BigDecimal(tokenText))
              if (value >= -2147483648 && value <= 2147483647) {
                value.toInt
              } else if (value >= -9223372036854775808L && value <= 9223372036854775807L) {
                value.toLong
              } else {
                value
              }
            }
          case JSONToken.STRING => JSONUtil.unescapeString(tokenText)
          case JSONToken.TRUE   => true
          case JSONToken.FALSE  => false
          case JSONToken.NULL   => null
          case JSONToken.RIGHT_BRACKET => // '}'
            if (stack.isEmpty) {
              throw new Exception(errorAtToken(token, s"Unexpected token '${tokenText}'"))
            }
            var objCnt = Stack[Any]()

            // pop until find '{'
            while (!isTokenType(stack.top, JSONToken.LEFT_BRACKET)) {
              objCnt.push(stack.top) // push pair
              if (stack.isEmpty) {
                throw new Exception(errorAtToken(token, s"missing '{'"))
              }
              stack.pop() // pop
            }
            stack.pop() // pop '{'

            var tupleList = Stack[(String, Any)]()

            while (!objCnt.isEmpty) {
              val p = objCnt.pop()
              p match {
                case Pair(key, value) => {
                  val pairP = p.asInstanceOf[Pair]
                  tupleList.push((pairP.key, pairP.value))
                }
                case _ => {
                  throw new Exception("object can only composed by pairs.")
                }
              }
              if (!objCnt.isEmpty) {
                expectToken(objCnt.top, JSONToken.COMMA)
                objCnt.pop()
              }
            }

            tupleList.toMap

          case JSONToken.RIGHT_PARAN => // ']'
            if (stack.isEmpty) {
              throw new Exception(errorAtToken(token, s"Unexpected token '${token.text}'"))
            }

            var arrCnt = Stack[Any]()

            // pop until find '['
            while (!isTokenType(stack.top, JSONToken.LEFT_PARAN)) {
              arrCnt.push(stack.top) // push item
              if (stack.isEmpty) {
                throw new Exception(errorAtToken(token, s"missing '['"))
              }
              stack.pop() // pop
            }
            stack.pop() // pop '['

            var list = ListBuffer[Any]()
            while (!arrCnt.isEmpty) {
              val v = arrCnt.pop()
              v match {
                case Pair(key, value) => {
                  throw new Exception("array can not contain pair.")
                }
                case _ => {
                  list.append(v)
                }
              }

              // check comma
              if (!arrCnt.isEmpty) {
                expectToken(arrCnt.top, JSONToken.COMMA)
                arrCnt.pop()
              }
            }

            list.toList
          case _ => token
        },
        path,
        parseReplacer
      )
    }

  private def cleanWipedData(stack: Stack[Any], token: JSONToken): Boolean =
    token.tokenType match {
      case JSONToken.RIGHT_PARAN => {
        if (stack.top == WIPE_VALUE) {
          stack.pop() // pop
        }
        false
      }
      case JSONToken.RIGHT_BRACKET => {
        if (stack.top == WIPE_VALUE) {
          stack.pop() // pop
        }
        false
      }
      case JSONToken.COMMA => {
        if (stack.top == WIPE_VALUE) {
          stack.pop() // pop
          true        // ignore current ,
        } else false
      }
      case _ => false
    }

  private def updatePath(path: Stack[PathNode], stack: Stack[Any], token: JSONToken) =
    token.tokenType match {
      case JSONToken.LEFT_PARAN => {
        path.push(PathNode(PathNode.ARRAY_CTX, "0"))
      }
      case JSONToken.LEFT_BRACKET => {
        path.push(PathNode(PathNode.OBJ_CTX, ""))
      }
      case JSONToken.RIGHT_PARAN   => path.pop()
      case JSONToken.RIGHT_BRACKET => path.pop()
      case JSONToken.COLON => {
        val top = path.pop()
        val key = stack.top.asInstanceOf[String]
        path.push(PathNode(PathNode.OBJ_CTX, key))
      }

      case JSONToken.COMMA => {
        val top = path.pop()
        if (top.ntype == PathNode.ARRAY_CTX) {
          path.push(PathNode(PathNode.ARRAY_CTX, top.index.toInt + 1 + ""))
        } else { // obj_ctx
          path.push(PathNode(PathNode.OBJ_CTX, ""))
        }
      }

      case _ => {}
    }

  private def reduceValue(stack: Stack[Any],
                          sourceValue: Any,
                          path: Stack[PathNode],
                          parseReplacer: ParseReplacer): Any =
    stack.push(
      if (sourceValue.isInstanceOf[JSONToken]) sourceValue
      else {
        val value = parseReplacer(sourceValue, path, stack)

        /**
          * When find a ':' at the top of stack, try to pair it with previous key, `key: value`
          */
        if (!stack.isEmpty && isTokenType(stack.top, JSONToken.COLON)) { // :
          val top = stack.top.asInstanceOf[JSONToken]
          stack.pop() // pop :

          if (stack.isEmpty) {
            throw new Exception(errorAtToken(top, s"Unexpected token '${top.text}'"))
          }
          val key = stack.pop() // pop

          if (!key.isInstanceOf[String]) {
            throw new Exception(
              errorAtToken(top, s"In a pair, key should be a string, but got $key")
            )
          }

          // find a pair
          Pair(key.asInstanceOf[String], value)
        } else {
          value
        }
      }
    )

  case class Pair(key: String, value: Any)

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
