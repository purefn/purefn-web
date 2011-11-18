package purefn.web

import scalaz._, Scalaz._

object Headers extends Headerss

trait Headerss {
  type Headers = Map[CaseInsensitive[String], NonEmptyList[String]]
  
  def addHeader[A](k: CaseInsensitive[String], v: String)(a: A)(implicit h: HasHeaders[A]): A = 
    h.updateHeaders(a)(_ |+| Map(k -> nels(v)))
    
  def setHeader[A](k: CaseInsensitive[String], v: String)(a: A)(implicit h: HasHeaders[A]): A =
    h.updateHeaders(a)(_ + (k -> nels(v)))
  
  def getHeaders[A](k: CaseInsensitive[String])(a: A)(implicit h: HasHeaders[A]): Option[NonEmptyList[String]] = 
    h.headers(a).get(k)
  
  def deleteHeader[A](k: CaseInsensitive[String])(a: A)(implicit h: HasHeaders[A]): A = 
    h.updateHeaders(a)(_ - k)
    
  implicit def RequestHasHeaders: HasHeaders[Request] = new HasHeaders[Request] {
    def updateHeaders(r: Request)(f: Headers => Headers) = r.copy(headers = f(r.headers))
    def headers(r: Request) = r.headers
  }

  implicit def ResponseHasHeaders: HasHeaders[Response] = new HasHeaders[Response] {
    def updateHeaders(r: Response)(f: Headers => Headers) = r.copy(headers = f(r.headers))
    def headers(r: Response) = r.headers
  }
  
  def showHeaders[A](a: A)(implicit h: HasHeaders[A]): Seq[String] = 
    h.headers(a).toSeq.map(kv => Seq(kv._1.original.shows, ": ", kv._2.shows).suml)
}

import Headers._

trait HasHeaders[A] {
  def updateHeaders(a: A)(f: Headers => Headers): A
  def headers(a: A): Headers
}