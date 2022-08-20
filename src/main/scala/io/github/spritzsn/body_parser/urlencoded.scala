package io.github.spritzsn.body_parser

import io.github.spritzsn.spritz.{DMap, HandlerResult, Machine, Request, RequestHandler2, Response, urlDecode}

import scala.collection.mutable

class FormParser extends Machine:
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
      case -1  => transition(FINAL)
      case '&' => transition(keyState)
      case '=' => parseError
      case c   => buf += c.toChar
    }

def urlencoded(): RequestHandler2 =
  (req: Request, res: Response) =>
    req.headers get "content-type" match
      case Some("application/x-www-form-urlencoded") =>
        val parser = new FormParser

        req.payload foreach (b => parser.send(b))
        parser.send(-1)
        req.body = new DMap(parser.data)
      case _ =>

    HandlerResult.Next
