package purefn.web

import scalaz._, Scalaz._, effect._

import Response._, Headers._

sealed case class Response(
  httpVersion: String = "1.1",
  status: Int = 200,
  statusReason: String = "Ok",
  contentLength: Option[Long] = None,
  headers: Headers = Map(),
  body: ResponseBody = new Forall[ResponseEnumT] { def apply[A] = mzero[ResponseEnumT[A]] }
)

object Response extends Responses

trait Responses {
  type ResponseEnumT[A] = EnumeratorT[Throwable, String, IO, A]
  type ResponseBody = Forall[ResponseEnumT]
  
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
      contentLength = some(body.length),
      body = new Forall[ResponseEnumT] { def apply[A] = enumStream(Stream(body)) } )
  }

  implicit def responseBodyStr(s: String): ResponseBody = new Forall[ResponseEnumT] {
    def apply[A] = enumStream(Stream(s))
  }
    
  implicit def ShowResponse: Show[Response] = shows { r =>
    def hdrs = Seq(
      Seq("headers:", "=============================="), 
      showHeaders(r),
      Seq("==============================")).suml
    def version = "version: " + r.httpVersion
    def status = "status: " + r.status
    def reason = "reason: " + r.statusReason
    def body = (hdrs ++ Seq(version, status, reason)).map(("    " + _) andThen (_ + "\n")).suml
    Seq("Response <\n", body, ">").suml
  }
}
