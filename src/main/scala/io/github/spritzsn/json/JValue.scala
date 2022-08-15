package io.github.spritzsn.json

import scala.collection.immutable.VectorMap

abstract class JValue:
  def number: String = asInstanceOf[JNumber].n
  def boolean: Boolean = asInstanceOf[JBoolean].b
  def string: String = asInstanceOf[JString].s
  def array: List[JValue] = asInstanceOf[JArray].a
  def obj: VectorMap[String, JValue] = asInstanceOf[JObject].o
  def nul: Null = { assert(this == JNull); null }

  def primitive: Any =
    this match
      case JNumber(n)  => n.toDouble
      case JBoolean(b) => b
      case JString(s)  => s
      case JNull       => null

case class JNumber(n: String) extends JValue
case class JBoolean(b: Boolean) extends JValue
case class JString(s: String) extends JValue
case object JNull extends JValue
case class JArray(a: List[JValue]) extends JValue
case class JObject(o: VectorMap[String, JValue]) extends JValue
