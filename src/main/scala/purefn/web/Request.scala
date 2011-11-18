package purefn.web

import scalaz._, Scalaz._, effect._

import Headers._

sealed case class Request(
  httpVersion: String = "1.1",
  pathInfo: String = "",
  headers: Headers = Map(),
  contextPath: String = "",
  params: Web.Params = Map()
)

object Request extends Requests

trait Requests {
  import Web._
  
  import scalaz.Show, Show._
  
  implicit def RequestShow: Show[Request] = shows { r =>
    def hdrs = Seq(
      Seq("headers:", "=============================="),
      showHeaders(r), 
      Seq("==============================")).suml
    def version = Seq("version: " + r.httpVersion)
    def pathInfo = Seq("path info: " + r.pathInfo)
    def contextPath = Seq("context path: " + r.contextPath)
    def params = Seq(
      Seq("params: ", "=============================="),
      r.params.toSeq.map(kv => Seq(kv._1.shows, ": ", kv._2.shows).suml),
      Seq("==============================")).suml
    def body = Seq(hdrs, version, pathInfo, contextPath, params).suml.map(("    " + _) andThen (_ + "\n")).suml
    Seq("Request <\n", body, ">").suml
  }
}
