//package io.github.spritzsn.example
//
//import cps.*
//import cps.monads.FutureAsyncMonad
//import scala.concurrent.duration.*
//import io.github.spritzsn.async._
//import io.github.spritzsn.spritz.Server
//import io.github.spritzsn.json.JSON
//
//@main def run(): Unit =
//  Server { app =>
//    app.use(JSON)
//    app.get("/", (req, res) => res.send("hello world"))
//    app.post("/", (req, res) => res.send(req.body))
//    app.listen(3000)
//    println("listening")
//  }
