package io.github.spritzsn.body_parser

import io.github.spritzsn.spritz.{DMap, HandlerResult, Machine, RequestHandler, urlDecode}

import scala.collection.mutable

def urlencoded(): RequestHandler =
  (req, res) =>
    req.headers get "content-type" match
      case Some("application/x-www-form-urlencoded") =>
        val parser = new BodyParser

        decompress(req) foreach (b => parser.send(b))
        parser.send(-1)
        req.body = new DMap(parser.data)
      case _ =>

    HandlerResult.Next

private class BodyParser extends Machine:
  val start: State = keyState
  var key: String = _
  val buf = new StringBuilder
  val data = new mutable.LinkedHashMap[String, Any]

  private def parseError = sys.error("error parsing body")

  case object keyState extends State:
    override def enter(): Unit = buf.clear()

    val on = {
      case -1 if buf.nonEmpty => parseError
      case -1                 => transition(FINAL)
      case '=' if buf.isEmpty => parseError
      case '=' =>
        key = urlDecode(buf.toString)
        transition(valueState)
      case '&' => parseError
      case c   => buf += c.toChar
    }

  case object valueState extends State:
    override def enter(): Unit = buf.clear()

    override def exit(): Unit = data(key) = urlDecode(buf.toString)

    val on = {
      case -1   => transition(FINAL)
      case '&'  => transition(keyState)
      case '\n' =>
      case '='  => parseError
      case c    => buf += c.toChar
    }
