package io.github.spritzsn.body_parser

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import io.github.spritzsn.spritz.{DMap, HandlerResult, RequestHandler2, Request, Response}

import scala.collection.immutable
import scala.collection.immutable.VectorMap
import scala.io.Codec

object JSON:
  def apply(): RequestHandler2 = new JSON

class JSON extends RequestHandler2:
  def apply(req: Request, res: Response): HandlerResult =
    req.headers get "content-type" match
      case Some("application/json") =>
        val json = new String(Codec.fromUTF8(req.payload))
        val obj = parse(json).eval.asInstanceOf[VectorMap[String, Any]] to mutable.LinkedHashMap

        req.body = new DMap(obj)
      case _ =>

    HandlerResult.Next

  private val HEX = {
    val a = Array.fill[Int](128)(-1)

    List(
      '0' -> 0,
      '1' -> 1,
      '2' -> 2,
      '3' -> 3,
      '4' -> 4,
      '5' -> 5,
      '6' -> 6,
      '7' -> 7,
      '8' -> 8,
      '9' -> 9,
      'a' -> 10,
      'A' -> 10,
      'b' -> 11,
      'B' -> 11,
      'c' -> 12,
      'C' -> 12,
      'd' -> 13,
      'D' -> 13,
      'e' -> 14,
      'E' -> 14,
      'f' -> 15,
      'F' -> 15,
    ) foreach { case (k, v) => a(k) = v }

    a to immutable.ArraySeq
  }

  private def hex(c: Char): Int =
    (if (c < 128) HEX(c) else -1) match
      case -1 => sys.error(s"expected hex digit, got $c")
      case d  => d

  private val EOI = '\uE000'

  def parse(json: String): JValue = {
    var idx: Int = 0

    def next: Char =
      if (idx > json.length) sys.error("past end of JSON string")
      else if (idx == json.length) EOI
      else json.charAt(idx)

    def ch: Char = {
      val c = next

      advance()
      c
    }

    def advance(): Unit = idx += 1

    def prev: Char = json.charAt(idx - 1)

    def space(): Unit = while (next.isWhitespace) advance()

    def chmatch(c: Char): Unit =
      if (ch != c)
        error(
          if (c == EOI) "expected end of input" else s"expected '$c', but found '$prev':\n$json\n${(" " * idx) :+ '^'}",
        )

    def delim(c: Char): Unit = {
      chmatch(c)
      space()
    }

    def readArray: JArray = {
      val buf = new ListBuffer[JValue]

      delim('[')

      @tailrec
      def elem(): Unit = {
        buf += readValue

        if (next == ',') {
          advance()
          space()
          elem()
        }
      }

      if (next != ']')
        elem()

      delim(']')
      JArray(buf toList)
    }

    def readObject: JObject = {
      val buf = new ListBuffer[(String, JValue)]

      delim('{')

      @tailrec
      def elem(): Unit = {
        val key = readString

        delim(':')

        buf += ((key, readValue))

        if (next == ',') {
          advance()
          space()
          elem()
        }
      }

      if (next != '}')
        elem()

      delim('}')
      JObject(buf to VectorMap)
    }

    def error(str: String) = sys.error(str)

    def readValue: JValue =
      next match {
        case `EOI`                      => error("unexpected end of JSON string")
        case '['                        => readArray
        case '{'                        => readObject
        case '"'                        => JString(readString)
        case d if d.isDigit || d == '-' => readNumber
        case 'n'                        => literal("null", JNull)
        case 't'                        => literal("true", JBoolean(true))
        case 'f'                        => literal("false", JBoolean(false))
      }

    def readString: String = {
      val buf = new StringBuilder

      chmatch('"')

      @tailrec
      def content(): Unit =
        ch match {
          case '\\' =>
            buf +=
              (ch match {
                case '\\' => '\\'
                case '"'  => '"'
                case '/'  => '/'
                case 'b'  => '\b'
                case 'f'  => '\f'
                case 'n'  => '\n'
                case 'r'  => '\r'
                case 't'  => '\t'
                case 'u'  => (hex(ch) << 12 | hex(ch) << 8 | hex(ch) << 4 | hex(ch)).toChar
              })
            content()
          case '"' =>
          case c =>
            buf += c
            content()
        }

      content()
      space()
      buf.toString
    }

    def readNumber: JNumber = {
      val buf = new StringBuilder
      var c: Char = next

      while (c.isDigit || c == '.' || c == '-' || c == 'e' || c == 'E') {
        buf += c
        advance()
        c = next
      }

      space()
      JNumber(buf.toString)
    }

    def literal(s: String, v: JValue): JValue = {
      for (i <- 0 until s.length) {
        if (next == EOI) error(s"unexpected end of JSON string: trying to match '$s'")
        else if (ch != s.charAt(i)) error(s"mismatch")
      }

      space()
      v
    }

    space()

    val v = readValue

    chmatch(EOI)
    v
  }
