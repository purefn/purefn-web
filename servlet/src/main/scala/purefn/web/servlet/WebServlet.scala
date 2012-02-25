package purefn.web
package servlet

import scalaz._, effect._, IO._

import Route.route

import javax.servlet.ServletConfig
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

abstract class WebServlet extends HttpServlet {
  private[this] val router: IORef[Web[Unit]] = newIORef(route[Web, Unit]()).unsafePerformIO
  
  def webInit(config: ServletConfig): IO[Web[Unit]]
  
  override final def init(config: ServletConfig) = initIO(config).unsafePerformIO
    
  override final def service(req: HttpServletRequest, res: HttpServletResponse) = 
    serviceIO(req, res).unsafePerformIO

  private def initIO(config: ServletConfig): IO[Unit] = webInit(config) flatMap (router.write(_))

  private def serviceIO(req: HttpServletRequest, res: HttpServletResponse): IO[Unit] = 
    for {
      r    <- router.read 
      res_ <- r.run(wrapReq(req))(throwIO(_)) map (_._2)
      _    <- setStatus(res, res_)
      _    <- setHeaders(res, res_)
      _    <- writeBody(res, res_)
    } yield ()
  
  private def wrapReq(r: HttpServletRequest) = 
    Request(
      httpVersion = r.getProtocol().substring(5)
    , method = r.getMethod
    , pathInfo = r.getPathInfo
    , contextPath = r.getContextPath
    , headers = headersFromServletReq(r)
    , params = paramsFromServletReq(r)
    )

  import std.string._
  import java.util.Enumeration
  import scala.collection.JavaConversions.enumerationAsScalaIterator
  private def headersFromServletReq(r: HttpServletRequest): Headers = {
    import purefn.web.{CaseInsensitive => CI}, NonEmptyList.nel
    
    r.getHeaderNames.asInstanceOf[Enumeration[String]].foldLeft(Map[CI[String], NonEmptyList[String]]()) { (hs, n) => 
      val vs = r.getHeaders(n).asInstanceOf[Enumeration[String]].toList
      if (!vs.isEmpty) hs + (CI(n) -> nel(vs.head, vs.tail)) else hs
    }
  }

  private def paramsFromServletReq(r: HttpServletRequest): Params =
    r.getHeaderNames.asInstanceOf[Enumeration[String]].foldLeft(Map[String, List[String]]()) { (hs, n) => 
      hs + (n -> r.getHeaders(n).asInstanceOf[Enumeration[String]].toList)
    }

  private def setStatus(sr: HttpServletResponse, wr: Response): IO[Unit] = IO(sr.setStatus(wr.status))
  
  private def setHeaders(sr: HttpServletResponse, wr: Response): IO[Unit] = 
    IO(wr.headers.foreach { case (h, vs) => vs.list.foreach(sr.addHeader(h.original, _)) })
  
  private def writeBody(sr: HttpServletResponse, wr: Response): IO[Unit] = {
    import iteratee.IterateeT.putStrTo

    (putStrTo[Throwable, String](sr.getOutputStream) >>== wr.body[Unit]) run (throwIO(_))
  }
}
