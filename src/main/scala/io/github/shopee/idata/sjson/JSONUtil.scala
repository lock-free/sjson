package io.github.free.lock.sjson

object JSONUtil {
  import scala.reflect._
  import scala.reflect.runtime.universe._

  def castType(value: Any, tpe: Type) = {
    val tpeClz = getClassByTpe(tpe)

    if (value == null) {
      value
    } else if (isSameType[Boolean](tpeClz, value, true) ||
               isSameType[Double](tpeClz, value, 0.0) ||
               isSameType[Float](tpeClz, value, 0.0f) ||
               isSameType[Long](tpeClz, value, 0l) ||
               isSameType[Int](tpeClz, value, 1) ||
               isSameType[Short](tpeClz, value, 1.toShort) ||
               isSameType[Byte](tpeClz, value, 1.toByte) ||
               isSameType[Char](tpeClz, value, 0.toChar)) {
      value
    } else if (tpeClz == classOf[Int] && value.isInstanceOf[Double]) {
      value.asInstanceOf[Double].toInt
    } else if (tpeClz == classOf[Int] && value.isInstanceOf[Long]) {
      value.asInstanceOf[Long].toInt
    } else if (tpeClz == classOf[Long] && value.isInstanceOf[Int]) {
      value.asInstanceOf[Int].toLong
    } else {
      try {
        tpeClz.cast(value)
      } catch {
        case e: Exception => {
          throw new Exception(
            s"Casting Fail! Value is $value, tpe is $tpe. Error message is ${e.getMessage}."
          )
        }
      }
    }
  }

  // hack
  def isSameType[T: TypeTag](tpeClz: Any, value: Any, sample: Any) =
    tpeClz == getClassByTpe(typeOf[T]) && (value != null && value.getClass == sample.getClass)

  def getTypeMembers(tpe: Type): Map[String, Type] =
    tpe.members.toList
      .filter(!_.isMethod)
      .map((item) => (item.name.toString().trim(), item.info))
      .toMap

  def getTypeParams(tpe: Type): List[Type] =
    tpe.dealias match { case TypeRef(_, _, args) => args }

  val mirror = runtimeMirror(getClass.getClassLoader)
  def getClassByTpe(tpe: Type) =
    try {
      if (tpe.typeSymbol.asClass == typeOf[Null].typeSymbol.asClass) {
        classOf[Null]
      } else if (tpe.typeSymbol.asClass == typeOf[Any].typeSymbol.asClass) {
        classOf[Any]
      } else {
        mirror.runtimeClass(tpe.typeSymbol.asClass)
      }
    } catch {
      case e: Exception => {
        throw new Exception(s"Fail to find class for tpe: $tpe. Error is ${e.getMessage}");
      }
    }

  // class
  def getFieldList(obj: Any) = {
    val fields = obj.getClass.getDeclaredFields
    fields
      .filter((item) => item.getName() != "$outer" && item.getName() != "MODULE$")
      .map(field => {
        val name = field.getName()
        try {
          field.setAccessible(true)
          val value: Any = field.get(obj)
          (name, value)
        } catch {
          case e: Exception => {
            throw new Exception(
              s"fail to get attribute $name of ${obj}. Error Message is: ${e.getMessage}"
            );
          }
        }
      })
      .toList
  }

  def invokeMethod(obj: Any, methodName: String) =
    try {
      obj.getClass.getDeclaredMethod(methodName).invoke(obj)
    } catch {
      case e: Exception => {
        throw new Exception(
          s"fail to invoke method ${methodName} in obj ${obj}. Error Message is: ${e.getMessage}"
        );
      }
    }

  def escapeString(txt: String) = {
    val txtBuilder = new StringBuilder
    var i          = 0
    val len        = txt.length
    while (i < len) {
      txtBuilder.append(escapedChar(txt(i)))
      i += 1
    }
    s""""${txtBuilder.toString()}""""
  }

  def isUnicode(txt: String, pos: Int): Boolean = {
    val txtLen = txt.length - 1
    if (pos + 6 > txtLen) return false
    //start iterating from /uD835 first hexadecimal character ('D' in this case)
    val hexString = txt.substring(pos + 2, pos + 6)
    val hexStringRegex = """[0-9a-fA-F]{4}"""
    if(hexString.matches(hexStringRegex)) return true
    false
  }

  def unescapeString(txt: String): String = {
    val txtBuilder = new StringBuilder // use txt builder to collect text

    var i   = 1
    var len = txt.length - 1

    while (i < len) {
      val ch = txt(i)
      if (ch == '\\') {
        val next = txt(i + 1)
        val newChar = next match {
          case 'b' => '\b'
          case 't' => '\t'
          case 'n' => '\n'
          case 'f' => '\f'
          case 'r' => '\r'
          case 'u' => 'u'
          case _   => next
        }
        if(newChar == 'u' && isUnicode(txt, i)) {
          val unicodeString = s"\\u${txt.substring(i + 2, i + 6)}"
          val unicodeChar = Integer.parseInt(unicodeString.drop(2), 16).toChar
          txtBuilder.append(unicodeChar)
          i += 6
        }
        else if (newChar == 'u') {
          txtBuilder.append("\\u")
          i += 2
        }
        else {
          txtBuilder.append(newChar)
          i += 2
        }
      } else {
        txtBuilder.append(ch)
        i += 1
      }
    }
    txtBuilder.toString()
  }

  private def escapedChar(ch: Char): String = ch match {
    case x if !shouldEncoding(x) => x.toString()
    case '"'                     => "\\\""
    case '\n'                    => "\\n"
    case '\\'                    => "\\\\"
    case '\f'                    => "\\f"
    case '\r'                    => "\\r"
    case '\b'                    => "\\b"
    case '\t'                    => "\\t"
    // '\\u hex hex hex hex'
    case x if x <= 0xF   => s"\\u000${Integer.toHexString(x)}"
    case x if x <= 0xFF  => s"\\u00${Integer.toHexString(x)}"
    case x if x <= 0xFFF => s"\\u0${Integer.toHexString(x)}"
    case x               => s"\\u${Integer.toHexString(x)}"
  }

  /**
    * unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
    */
  private def shouldEncoding(ch: Char): Boolean =
    ch match {
      case '"'  => true
      case '\\' => true
      case x    => x < 0x20
    }
}
