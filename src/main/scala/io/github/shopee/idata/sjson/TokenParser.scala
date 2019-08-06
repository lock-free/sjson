package io.github.free.lock.sjson

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

case class Transition(nextState: Int, isEpsilon: Boolean = false)

object TransitionState {
  // states
  val NEW = 0

  val NULL_N = 1
  val NULL_U = 2
  val NULL_L = 3

  val TRUE_T = 4
  val TRUE_R = 5
  val TRUE_U = 6

  val FALSE_F = 7
  val FALSE_A = 8
  val FALSE_L = 9
  val FALSE_S = 10

  val STRING_IN     = 11
  val STRING_ESCAPE = 12

  val NUMBER_INTEGER          = 13
  val NUMBER_FRAGMENT_INTEGER = 14
  val NUMBER_SCIENCE_PART     = 15
  val NUMBER_SCIENCE_INTEGER  = 16

  // transitions
  val NEW_TRANS                      = Transition(NEW)
  val NEW_TRANS_E                    = Transition(NEW, true)
  val NULL_N_TRANS                   = Transition(NULL_N)
  val NULL_U_TRANS                   = Transition(NULL_U)
  val NULL_L_TRANS                   = Transition(NULL_L)
  val TRUE_T_TRANS                   = Transition(TRUE_T)
  val TRUE_R_TRANS                   = Transition(TRUE_R)
  val TRUE_U_TRANS                   = Transition(TRUE_U)
  val FALSE_F_TRANS                  = Transition(FALSE_F)
  val FALSE_A_TRANS                  = Transition(FALSE_A)
  val FALSE_L_TRANS                  = Transition(FALSE_L)
  val FALSE_S_TRANS                  = Transition(FALSE_S)
  val STRING_ESCAPE_TRANS            = Transition(STRING_ESCAPE)
  val STRING_IN_TRANS                = Transition(STRING_IN)
  val NUMBER_INTEGER_TRANS           = Transition(NUMBER_INTEGER)
  val NUMBER_SCIENCE_INTEGER_TRANS   = Transition(NUMBER_SCIENCE_INTEGER)
  val NUMBER_SCIENCE_INTEGER_TRANS_E = Transition(NUMBER_SCIENCE_INTEGER, true)
  val NUMBER_FRAGMENT_INTEGER_TRANS  = Transition(NUMBER_FRAGMENT_INTEGER)
  val NUMBER_SCIENCE_PART_TRANS      = Transition(NUMBER_SCIENCE_PART)
}

class ParseToken(onTokenCallback: (JSONToken) => Unit) {
  import TransitionState._

  val txtBuilder = new StringBuilder // use txt builder to collect text
  var strClosed  = false
  var state: Int = NEW

  /**
    * change current state by ch
    */
  def transist(ch: Char, i: Int): Transition = {
    val transis = state match {
      // null
      case NULL_N => if (ch == 'u') NULL_U_TRANS else throwParseErr(ch, i)
      case NULL_U => if (ch == 'l') NULL_L_TRANS else throwParseErr(ch, i)
      case NULL_L => {
        if (ch == 'l') {
          onToken(JSONToken(JSONToken.NULL, "null", i - 3))
        } else throwParseErr(ch, i)
      }
      // true
      case TRUE_T => if (ch == 'r') TRUE_R_TRANS else throwParseErr(ch, i)
      case TRUE_R => if (ch == 'u') TRUE_U_TRANS else throwParseErr(ch, i)
      case TRUE_U => {
        if (ch == 'e') {
          onToken(JSONToken(JSONToken.TRUE, "true", i - 3))
        } else {
          throwParseErr(ch, i)
        }
      }
      // false
      case FALSE_F => if (ch == 'a') FALSE_A_TRANS else throwParseErr(ch, i)
      case FALSE_A => if (ch == 'l') FALSE_L_TRANS else throwParseErr(ch, i)
      case FALSE_L => if (ch == 's') FALSE_S_TRANS else throwParseErr(ch, i)
      case FALSE_S => {
        if (ch == 'e') {
          onToken(JSONToken(JSONToken.FALSE, "false", i - 4))
        } else throwParseErr(ch, i)
      }

      case STRING_IN => {
        if (ch == '\\') {
          txtBuilder.append(ch)
          STRING_ESCAPE_TRANS
        } else if (ch == '"') {
          strClosed = true
          txtBuilder.append('"')
          onToken(JSONToken(JSONToken.STRING, txtBuilder.toString(), i + 1 - txtBuilder.length))
        } else {
          txtBuilder.append(ch)
          STRING_IN_TRANS
        }
      }

      case STRING_ESCAPE => {
        txtBuilder.append(ch)
        STRING_IN_TRANS
      }

      case NUMBER_INTEGER => {
        // integer part, 01234
        if (ch >= '0' && ch <= '9') {
          txtBuilder.append(ch)
          NUMBER_INTEGER_TRANS
        } else {
          // fragment part, .01234
          if (ch == '.') {
            txtBuilder.append(ch)
            NUMBER_FRAGMENT_INTEGER_TRANS
          } else if (ch == 'e' || ch == 'E') {
            txtBuilder.append(ch)
            NUMBER_SCIENCE_PART_TRANS
          } else {
            val numberText = txtBuilder.toString()
            onToken(JSONToken(JSONToken.NUMBER, numberText, i - numberText.length), true)
          }
        }
      }

      case NUMBER_FRAGMENT_INTEGER => {
        // integers
        if (ch >= '0' && ch <= '9') {
          txtBuilder.append(ch)
          NUMBER_FRAGMENT_INTEGER_TRANS
        } else {
          if (ch == 'e' || ch == 'E') {
            txtBuilder.append(ch)
            NUMBER_SCIENCE_PART_TRANS
          } else {
            val numberText = txtBuilder.toString()
            onToken(JSONToken(JSONToken.NUMBER, numberText, i - numberText.length), true)
          }
        }
      }

      case NUMBER_SCIENCE_PART => {
        if (ch == '+' || ch == '-') {
          txtBuilder.append(ch)
          NUMBER_SCIENCE_PART_TRANS
        } else if (ch >= '0' && ch <= '9') {
          NUMBER_SCIENCE_INTEGER_TRANS_E
        } else {
          throw new Exception(s"expect number after '+', but got ${ch}")
        }
      }

      case NUMBER_SCIENCE_INTEGER => {
        if (ch >= '0' && ch <= '9') {
          txtBuilder.append(ch)
          NUMBER_SCIENCE_INTEGER_TRANS
        } else {
          val numberText = txtBuilder.toString()
          if (numberText == "-") {
            throw new Exception(tokenParseError(ch, i, "expect number after '-'"))
          }

          onToken(JSONToken(JSONToken.NUMBER, numberText, i - 1 - numberText.length), true)
        }
      }

      case NEW => {
        ch match {
          case '{'  => singleChToken(JSONToken.LEFT_BRACKET, ch, i)
          case '}'  => singleChToken(JSONToken.RIGHT_BRACKET, ch, i)
          case '['  => singleChToken(JSONToken.LEFT_PARAN, ch, i)
          case ']'  => singleChToken(JSONToken.RIGHT_PARAN, ch, i)
          case ','  => singleChToken(JSONToken.COMMA, ch, i)
          case ':'  => singleChToken(JSONToken.COLON, ch, i)
          case ' '  => NEW_TRANS // white space
          case '\t' => NEW_TRANS // white space
          case '\r' => NEW_TRANS // white space
          case '\n' => NEW_TRANS // white space
          case '\f' => NEW_TRANS // white space
          case '\b' => NEW_TRANS // white space

          case _ if (ch == 't') => TRUE_T_TRANS
          case _ if (ch == 'f') => FALSE_F_TRANS
          case _ if (ch == 'n') => NULL_N_TRANS

          case '"' => {
            txtBuilder.setLength(0)
            strClosed = false
            txtBuilder.append("\"")
            STRING_IN_TRANS
          }

          case _ if (ch == '-' || (ch >= '0' && ch <= '9')) => {
            txtBuilder.setLength(0)
            txtBuilder.append(ch)
            NUMBER_INTEGER_TRANS
          }

          case _ =>
            throw new Exception(
              tokenParseError(
                ch,
                i,
                s"unrecorgnized symbol '$ch', the int value of char is ${ch.toInt}. Current state is ${state}"
              )
            )
        }
      }
    }
    state = transis.nextState
    transis
  }

  def transistEnd(location: Int) =
    // at last
    if (state == NUMBER_INTEGER || state == NUMBER_FRAGMENT_INTEGER || state == NUMBER_SCIENCE_INTEGER) {
      val numberText = txtBuilder.toString()
      if (numberText == "-") {
        throw new Exception("expect number after '-'")
      }

      val token = JSONToken(JSONToken.NUMBER, numberText, 1 + location - numberText.length)
      onToken(token)
    } else if (state != NEW) {
      throw new Exception(
        s"parse error at last, current state is ${state}. Current text buffer is ${txtBuilder.toString()}."
      )
    }

  private def singleChToken(t: Int, ch: Char, i: Int): Transition =
    onToken(JSONToken(t, ch + "", i))

  private def onToken(token: JSONToken, isEpsilon: Boolean = false): Transition = {
    onTokenCallback(token)
    if (isEpsilon) {
      NEW_TRANS_E
    } else {
      NEW_TRANS
    }
  }

  private def throwParseErr(ch: Char, location: Int) =
    throw new Exception(
      tokenParseError(ch,
                      location,
                      s"unrecorgnized symbol '$ch', the int value of char is ${ch.toInt}.")
    )

  private def tokenParseError(ch: Char, location: Int, errorMessage: String): String =
    s"""[${location}]${errorMessage}. Error happened nearby '${ch}'."""
}

object TokenParser {
  def toTokens(jsonTxt: String): List[JSONToken] = {
    var i             = 0
    val jsonTxtLen    = jsonTxt.length
    val tokensBuilder = ListBuffer[JSONToken]()

    var parseToken = new ParseToken((token) => {
      tokensBuilder.append(token)
    })

    while (i < jsonTxtLen) {
      val ch    = jsonTxt(i)
      val trans = parseToken.transist(ch, i)
      if (!trans.isEpsilon) {
        i += 1
      }
    }

    parseToken.transistEnd(i - 1)

    tokensBuilder.toList
  }

  def toTokenAsyncIterator(iter: AsyncIterator[Char]): AsyncIterator[JSONToken] = {
    val tokenIter = new AsyncIterator[JSONToken]()

    var parseToken = new ParseToken((token) => {
      tokenIter.push(token)
    })

    iter.forEach(
      itemHandler = (ch, i) => {
        var trans = parseToken.transist(ch, i)
        while (trans.isEpsilon) {
          trans = parseToken.transist(ch, i)
        }
      },
      resultCallback = ResultCallback[Null](
        endCallback = (prev) => {
          parseToken.transistEnd(iter.getIndex() - 1)
          tokenIter.end()
        },
        errorCallback = (err) => {
          tokenIter.error(err)
        }
      )
    )

    tokenIter
  }
}
