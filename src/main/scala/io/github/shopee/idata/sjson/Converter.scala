package io.github.free.lock.sjson

import scala.reflect._
import scala.reflect.runtime.universe._

/**
  * convert a plain scala value (string, number, map, list, true, false, null) to a specific case class
  */
object JSONConverter {
  def convert[T: TypeTag](plain: Any) =
    convertHelp(typeOf[T], plain).asInstanceOf[T]

  def convertHelp(tpe: Type, plain: Any): Any = {
    val runtimeClz = JSONUtil.getClassByTpe(tpe)

    if (runtimeClz == classOf[List[Any]] ||
        runtimeClz == classOf[Array[Any]] ||
        runtimeClz == classOf[Vector[Any]]) { // List
      val list       = convertPlainToList(plain)
      val typeParams = JSONUtil.getTypeParams(tpe)
      val itemTpe    = typeParams(0)
      val ret        = list.map((item) => convertHelp(itemTpe, item))

      if (runtimeClz == classOf[Vector[Any]]) {
        ret.toVector
      } else if (runtimeClz == classOf[Array[Any]]) {
        ret.toArray
      } else ret
    } else if (runtimeClz == classOf[Map[String, Any]] ||
               runtimeClz == classOf[scala.collection.mutable.Map[String, Any]]) { // Map
      val map = convertPlainToMap(plain)

      val types  = JSONUtil.getTypeParams(tpe)
      val keyTpe = types(0)
      if (JSONUtil.getClassByTpe(keyTpe) != classOf[String]) {
        throw new Exception("Object in json can only use string as key.")
      }

      val valueTpe = types(1)
      val ret = map.toList
        .map((item) => {
          (item._1, convertHelp(valueTpe, item._2))
        })
        .toMap

      if (runtimeClz == classOf[scala.collection.mutable.Map[String, Any]])
        scala.collection.mutable.Map(ret.toSeq: _*)
      else ret
    } else if (runtimeClz == classOf[Null] ||
               runtimeClz == classOf[Nothing]) { // regard null and none the same
      if (plain == None || plain == null) null
      else {
        throw new Exception("Expect null or none");
      }
    } else if (runtimeClz == classOf[Any] || runtimeClz == classOf[AnyRef]) {
      plain
    } else if (isMap(plain)) { // class
      val obj        = convertPlainToMap(plain)
      val memberTyps = JSONUtil.getTypeMembers(tpe)

      val fields = runtimeClz.getDeclaredFields
      if (fields.exists((field) => field.getName() == "$outer")) {
        throw new Exception(
          s"Do not define your case class inside Object or Class. Fail to convert json object $obj to class $runtimeClz."
        );
      }
      val types = fields.map((field) => field.getType())
      val values: Array[AnyRef] = fields.zipWithIndex.map((item) => {
        val (field, i) = item
        val propName   = field.getName()
        val propTpe    = memberTyps(propName)
        val propPlain =
          if (obj.exists(_._1 == propName)) obj(propName)
          else {
            val defValMethod = defaultValueInitFieldName(i)
            try {
              runtimeClz.getMethod(defValMethod).invoke(null)
            } catch {
              case e: Exception => {
                throw new Exception(
                  s"Missing property $propName, and there is no default value for it in case class. ${e.getMessage}"
                )
              }
            }
          }

        convertHelp(propTpe, propPlain).asInstanceOf[AnyRef]
      });

      // static apply class for case class
      try {
        val method = runtimeClz.getMethod("apply", types: _*)
        method.invoke(null, values: _*)
      } catch {
        case e: Exception => {
          throw new Exception(
            s"""Fail to invoke apply function of you case class. Please make sure you defined case class which would have a default apply function. Fail to convert json object '$obj' to class '$runtimeClz'. Values for apply: ${values.toList}. Error message: ${e.getMessage}."""
          );
        }
      }
    } else {
      JSONUtil.castType(plain, tpe)
    }
  }

  private def defaultValueInitFieldName(i: Int): String =
    s"$$lessinit$$greater$$default$$${i + 1}"

  private def convertPlainToMap(plain: Any) =
    plain match {
      case v: scala.collection.mutable.Map[String, Any]   => v
      case v: scala.collection.immutable.Map[String, Any] => v
      case _ => {
        throw new Exception("can not convert value to map.")
      }
    }

  private def isMap(plain: Any) =
    plain.isInstanceOf[scala.collection.mutable.Map[String, Any]] ||
    plain.isInstanceOf[scala.collection.immutable.Map[String, Any]]

  private def convertPlainToList(plain: Any) =
    plain match {
      case v: List[Any]   => v
      case v: Vector[Any] => v
      case _ => {
        throw new Exception(s"can not convert value to list: $plain, ${plain.getClass}")
      }
    }
}
