package purefn.web
package example

import PathTemplate._

import scalaz._, Scalaz._
import std.anyVal._
import std.string._
import typelevel._, Typelevel._

object PathTemplateUsage extends App {
  val hello = "hello" /: $
  val fib = "fib" /: Param[Int]("n") /: $
  val greeting = "hello" /: Param[String]("greeting") /: **

  hello.toURI(HNil) assert_=== "hello"
  greeting.toURI("to all the" :: List("dudes and dudettes", "in the world") :: HNil) assert_=== "hello/to+all+the/dudes+and+dudettes/in+the+world"
  fib.toURI(50 :: HNil) assert_=== "fib/50"
    
  
}

