package io.github.free.lock.sjson

import reflect.runtime.universe._

import scala.collection.mutable.Stack
import scala.collection.AbstractSeq

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
  val WIPE_VALUE = JSONParser.WIPE_VALUE

  def parse(jsonTxt: String,
            parseReplacer: JSONParser.ParseReplacer = JSONParser.defParseReplacer) =
    JSONParser.parse(jsonTxt, parseReplacer)

  def parseAsyncIterator(textIter: AsyncIterator[Char],
                         parseReplacer: JSONParser.ParseReplacer = JSONParser.defParseReplacer) =
    JSONParser.parseAsyncIterator(textIter, parseReplacer)

  def convert[T: TypeTag](plain: Any) = JSONConverter.convert[T](plain)

  def parseTo[T: TypeTag](jsonTxt: String,
                          parseReplacer: JSONParser.ParseReplacer = JSONParser.defParseReplacer) =
    convert[T](parse(jsonTxt, parseReplacer))

  def stringify(
      obj: Any,
      replacer: Stringify.StringifyReplacer = Stringify.defaultStringifyReplacer
  ): String =
    Stringify.stringify(obj, replacer)

  def toJsonPath(path: Stack[PathNode]): String = JSONParser.toJsonPath(path)
}
