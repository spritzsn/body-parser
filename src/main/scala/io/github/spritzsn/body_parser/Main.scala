//package io.github.spritzsn.body_parser
//
//import cps.*
//import cps.monads.FutureAsyncMonad
//
//import scala.concurrent.duration.*
//import io.github.spritzsn.async.*
//import io.github.spritzsn.spritz.{Request, Response, Server}
//
//@main def run(): Unit =
//  Server { app =>
//    app.use(JSON())
//    app.get("/", (req: Request, res: Response) => res.send("hello world"))
//    app.post("/", (req: Request, res: Response) => res.send(req.body))
//    app.listen(3000)
//    println("listening")
//  }
