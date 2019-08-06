package io.github.free.lock.sjson

import reflect.runtime.universe._

import scala.collection.mutable.Stack
import scala.collection.AbstractSeq
import java.util.IdentityHashMap

object Stringify {
  // (current object, json path) -> Option[result]
  type StringifyReplacer = (Any, Stack[String]) => Option[String]

  def defaultStringifyReplacer(data: Any, path: Stack[String]): Option[String] = None

  def stringify(obj: Any, replacer: StringifyReplacer = defaultStringifyReplacer): String =
    stringifyHelp(obj, Stack[String](), new IdentityHashMap[Any, Int](), replacer)

  private def stringifyHelp(data: Any,
                            path: Stack[String],
                            idMap: IdentityHashMap[Any, Int],
                            replacer: StringifyReplacer): String =
    replacer(data, path) match {
      case Some(text) => text
      case None => // None would means no replacer for this object
        data match {
          // string
          case v: String             => JSONUtil.escapeString(v)
          case v: Char               => JSONUtil.escapeString(v.toString())
          case v: java.sql.Timestamp => JSONUtil.escapeString(v.toString())
          case v: java.util.Date     => JSONUtil.escapeString(v.toString())

          // number
          case v: Double => if (v.isNaN || v.isInfinity) "null" else v.toString()
          case v: Float  => if (v.isNaN || v.isInfinity) "null" else v.toString()
          case v: Number => v.toString()

          // boolean
          case v: Boolean => if (v == true) "true" else "false"
          // null
          case None    => "null"
          case null    => "null"
          case v: Unit => "null"

          // array and object
          case _ => {
            if (idMap.containsKey(data)) {
              val pathSeq = path.toList.reverse
              val idx     = idMap.get(data)
              throw new Exception(s"circle detected! value('${pathSeq
                .mkString(".")}') is the same as value('${pathSeq.slice(0, idx).mkString(".")}').")
            }
            idMap.put(data, path.length)
            val ret = data match {
              // array
              case v: AbstractSeq[_] => stringifyList(v, path, idMap, replacer)
              case v: scala.collection.mutable.AbstractSeq[_] =>
                stringifyList(v.toList, path, idMap, replacer)
              case v: Array[_] => stringifyList(v.toList, path, idMap, replacer)

              // object
              case v: scala.collection.mutable.Map[_, _] =>
                stringifyTupleList(v.toList, path, idMap, replacer)
              case v: scala.collection.immutable.Map[_, _] =>
                stringifyTupleList(v.toList, path, idMap, replacer)

              // class
              case _ => stringifyTupleList(JSONUtil.getFieldList(data), path, idMap, replacer)
            }
            idMap.remove(data)
            ret
          }
        }
    }

  private def stringifyList(v: AbstractSeq[Any],
                            path: Stack[String],
                            idMap: IdentityHashMap[Any, Int],
                            replacer: StringifyReplacer) =
    s"[${v.zipWithIndex
      .map((tup) => {
        path.push(tup._2 + "")
        val ret = stringifyHelp(tup._1, path, idMap, replacer)
        path.pop()
        ret
      })
      .mkString(",")}]"

  private def stringifyTupleList(tupleList: List[(Any, Any)],
                                 path: Stack[String],
                                 idMap: IdentityHashMap[Any, Int],
                                 replacer: StringifyReplacer) =
    s"{${tupleList
      .map((item) => {
        val keyText = item._1.toString()
        val key     = JSONUtil.escapeString(keyText)
        path.push(keyText)
        val value = stringifyHelp(item._2, path, idMap, replacer)
        path.pop()
        s"$key:$value"
      })
      .mkString(",")}}"
}
