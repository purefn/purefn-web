package purefn.web.examples.apparatus

import purefn.web._, Response._, PathTemplate._, Route._
import purefn.web.apparatus.{WebResource => wr, _}, wr.{getRequest => _, _}

import scalaz._, scalaz.iteratee._, effect._
import NonEmptyList._
import syntax.pointed._
import syntax.show._
import syntax.std.stringV._
import std.anyVal._
import std.string._

sealed trait Config {
  val name: String
}

object WebApparatusUsage {
  type AppWebFn[A] = WebFn[Config, A]
   
  def hello: AppWebFn[Unit] = {
    case class Say(greeting: String)
    
    def init: InitFn[Config, Say] = Kleisli(c => Say("Hello, world! From " + c.name).point[WebState])
    
    def toHtml: BodyProducingFn[Say] = for {
      c <- wr.getContext
      r <- wr.getRequest
    } yield r.contextPath + r.pathInfo + "> " + r.params.get("greeting").getOrElse(c.greeting)
    
    webResource(init = init, contentTypesProvided = "text/html" -> toHtml)
  }
 
  def fibonacci: AppWebFn[Unit] = {
    
    def init: InitFn[Config, Unit] = Kleisli(c => ().point[WebState])
    lazy val fib: Stream[Long] = Stream.cons(1, Stream.cons(2, fib.zip(fib.tail).map(x => x._1 + x._2)))
    
    def toHtml: BodyProducingFn[Unit] = for {
      r <- wr.getRequest
    } yield { 
      val a = r.params.get("n").flatMap(_.headOption).map(_.parseInt.fold(_.getMessage, fib(_).shows)).getOrElse("missing parameter")
      a
    }
    
    webResource(init = init, contentTypesProvided = "text/html" -> toHtml)
  }

  val helloPt= "hello" /: Param[String]("greeting") /: **
  val fibPt= "fib" /: Param[Int]("n") /: **
  val router = route(
      **           -> hello
    , "hello" /: $ -> hello
    , helloPt      -> hello
    , fibPt        -> fibonacci
    )

  def main(args: Array[String]): Unit = mainIO(args) unsafePerformIO 
  
  def mainIO(args: Array[String]): IO[Unit] = {
    import IO._
    import syntax.bind._
    import IterateeT._
    
    val config = new Config { val name = "Usage Example!" }
    val request = Request(
        method = "GET"
      , pathInfo = "hello"
      , headers = Map(CaseInsensitive("Accept") -> nels("text/html"))
      )

    router(config).run(request)(throwIO(_)) flatMap  (rr =>
      // write response to servlet response
      putStrLn(rr._1.shows) >>
        putStrLn(rr._2.shows) >>
        putStrLn("Response body") >>
        putStrLn("=====================================") >>
//        ((putStrTo[Throwable, String](System.out) >>== rr._2.body[Unit]) apply (throwIO(_))) >>
        putStrLn("") >>
        putStrLn("=====================================")
      )
  }
}

