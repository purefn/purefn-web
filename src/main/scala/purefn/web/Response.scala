package purefn.web

import scalaz._
import syntax.monoid._
import iteratee.EnumeratorT.enumStream

sealed case class Response(
  httpVersion: String = "1.1",
  status: Int = 200,
  statusReason: String = "Ok",
  contentLength: Option[Long] = None,
  headers: Headers = Map(),
  body: ResponseBody = new Forall[ResponseEnumT] { def apply[A] = mzero[ResponseEnumT[A]] }
)

object Response extends ResponseFunctions with ResponseInstances

trait ResponseFunctions {
  def emptyResponse: Response = Response()
  
  import Web._
  
  def setContentLength(l: Long): Response => Response = _.copy(contentLength = Some(l))
  
  def setResponseStatus(s: Int, reason: String): Response => Response = _.copy(status = s, statusReason = reason)
  
  def modifyResponseBody(f: ResponseEnumT ~> ResponseEnumT): Response => Response = r => 
    r.copy(body = new Forall[ResponseEnumT] {
      def apply[A] = f(r.body[A])
    })

  def fourOhFour = {
    val body = "<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>" +
      "The requested document was not found on this server.<p><hr></body></html>"
    Response(
      status = 404,
      statusReason = "Not Found",
      contentLength = Some(body.length),
      body = new Forall[ResponseEnumT] { def apply[A] = enumStream(Stream(body)) } )
  }
  
  def fourOhSix: Response = 
    Response(
      status = 406,
      statusReason = "Not Acceptable",
      contentLength = Some(0)
    )

  implicit def responseBodyStr(s: String): ResponseBody = new Forall[ResponseEnumT] {
    def apply[A] = enumStream(Stream(s))
  }
}

trait ResponseInstances {    
  implicit def ShowResponse: Show[Response] = new Show[Response] {
    import Headers._
    import syntax.show._
    import std.string._
    import std.list._
    import syntax.foldable._

    def show(r: Response) = shows(r).toList
    
    override def shows(r: Response) = {
      def hdrs = List(
        List("headers:", "=============================="), 
        showHeaders(r),
        List("==============================")).flatten
      def version = "version: " + r.httpVersion
      def status = "status: " + r.status
      def reason = "reason: " + r.statusReason
      def body = (hdrs ++ List(version, status, reason)).map(("    " + _) andThen (_ + "\n")).foldMap()
      List("Response <\n", body, ">").foldMap()
    }
  }

  implicit def ResponseHasHeaders: HasHeaders[Response] = new HasHeaders[Response] {
    def updateHeaders(r: Response)(f: Headers => Headers) = r.copy(headers = f(r.headers))
    def headers(r: Response) = r.headers
  }
}
