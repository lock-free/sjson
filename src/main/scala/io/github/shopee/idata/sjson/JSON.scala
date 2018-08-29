package io.github.shopee.idata.sjson

import reflect.runtime.universe._

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

/**
  *
  * JSON parser for scala
  *
  * JSON data structure:
  *   number,
  *   string
  *   true/false/null,
  *   object,
  *   array
  *
  * 1. general data structure for json-style
  *
  * (1) general type mapping between json-scala
  *    number - Double
  *    string - String
  *    true - true
  *    false - false
  *    object - Map
  *    array - List
  *
  * 2. specific class
  *
  * 3. convert scala object to json string
  *
  * 4. convert json string to scala object
  */
object JSON {
  case class SNode(data: Any, var visited: Boolean = false);
  case class SPair(key: String, value: Any);

  def stringify(obj: Any): String =
    obj match {
      case v: Unit => "null"
      case _       => stringifyHelp(obj)
    }

  def parse(jsonTxt: String) = JSONParser.parse(jsonTxt)

  def convert[T: TypeTag](plain: Any) = JSONConverter.convert[T](plain)

  def parseTo[T: TypeTag](jsonTxt: String) =
    JSONConverter.convert[T](parse(jsonTxt))

  private def stringifyHelp(data: Any): String =
    data match {
      // string
      case v: String => JSONUtil.escapeString(v)
      // number
      case v: Number => v.toString()

      case v: java.sql.Timestamp => JSONUtil.escapeString(v.toString())
      case v: java.sql.Date      => JSONUtil.escapeString(v.toString())
      // boolean
      case v: Boolean => if (v == true) "true" else "false"
      // null and none
      case None => "null"
      case null => "null"

      // array
      case v: List[Any] => {
        s"[${v.map((item) => stringifyHelp(item)).mkString(",")}]"
      }
      case v: Array[Any] => {
        s"[${v.map((item) => stringifyHelp(item)).mkString(",")}]"
      }
      case v: Vector[Any] => {
        s"[${v.map((item) => stringifyHelp(item)).mkString(",")}]"
      }

      // object
      case v: scala.collection.mutable.Map[String, Any] => {
        s"{${v.toList.map((item) => s"${stringifyHelp(item._1)}:${stringifyHelp(item._2)}").mkString(",")}}"
      }
      case v: scala.collection.immutable.Map[String, Any] => {
        s"{${v.toList.map((item) => s"${stringifyHelp(item._1)}:${stringifyHelp(item._2)}").mkString(",")}}"
      }

      // class
      case _ => {
        // TODO circle detection
        s"{${JSONUtil.getFieldList(data).map((item) => s"${stringifyHelp(item._1)}:${stringifyHelp(item._2)}").mkString(",")}}"
      }
    }
}
