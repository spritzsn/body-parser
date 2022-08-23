package io.github.spritzsn.body_parser

import io.github.spritzsn.spritz.Request

import scala.collection.mutable.ArrayBuffer

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

def decompress(req: Request): Array[Byte] =
  req.headers get "content-encoding" match
    case Some("gzip")   => gzipDecompress(req.payload)
    case Some(encoding) => sys.error(s"encoding '$encoding' not supported")
    case _              => req.payload

def gzipDecompress(input: Array[Byte]): Array[Byte] =
  val buf = new ArrayBuffer[Byte]
  val arr = new Array[Byte](1024)
  val is = new ByteArrayInputStream(input)
  val iis = new GZIPInputStream(is)

  while iis.available > 0 do buf ++= arr.view.take(iis.read(arr))

  buf.toArray
