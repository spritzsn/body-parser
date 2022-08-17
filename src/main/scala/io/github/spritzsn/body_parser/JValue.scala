package io.github.spritzsn.body_parser

import scala.collection.immutable.VectorMap

abstract class JValue:
  def number: String = asInstanceOf[JNumber].n
  def boolean: Boolean = asInstanceOf[JBoolean].b
  def string: String = asInstanceOf[JString].s
  def array: List[JValue] = asInstanceOf[JArray].l
  def obj: VectorMap[String, JValue] = asInstanceOf[JObject].o
  def nul: Null = { assert(this == JNull); null }

  def eval: Any =
    def eval(v: JValue): Any =
      v match
        case JNumber(n)  => n.toDouble
        case JBoolean(b) => b
        case JString(s)  => s
        case JNull       => null
        case JArray(l)   => l map eval
        case JObject(o)  => o.view.mapValues(eval) to VectorMap

    eval(this)

case class JNumber(n: String) extends JValue
case class JBoolean(b: Boolean) extends JValue
case class JString(s: String) extends JValue
case object JNull extends JValue
case class JArray(l: List[JValue]) extends JValue
case class JObject(o: VectorMap[String, JValue]) extends JValue
