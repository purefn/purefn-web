package purefn.web

sealed case class Request(
  httpVersion: String = "1.1",
  pathInfo: String = "",
  headers: Headers = Map(),
  contextPath: String = "",
  params: Params = Map()
)

object Request extends RequestInstances

trait RequestInstances {
  import Web._
  import scalaz._
  
  implicit def RequestShow: Show[Request] = new Show[Request] {
    import Headers._
    import syntax.show._
    import std.string._
    import std.list._
    import syntax.foldable._
    
    def show(r: Request) = shows(r).toList
    
    override def shows(r: Request) = {
      def hdrs = List(
        List("headers:", "=============================="),
        showHeaders(r), 
        List("==============================")).flatten
      def version = List("version: " + r.httpVersion)
      def pathInfo = List("path info: " + r.pathInfo)
      def contextPath = List("context path: " + r.contextPath)
      def params = List(
        List("params: ", "=============================="),
        r.params.toList.flatMap(kv => List(kv._1.shows, ": ", kv._2.toList.shows)),
        List("==============================")).flatten
      def body: String =
        List(hdrs, version, pathInfo, contextPath, params).flatMap(_.map(("    " + _) andThen (_ + "\n"))).foldMapIdentity     
      List("Request <\n", body, ">").foldMapIdentity
      
      
    }
  }

  implicit def RequestHasHeaders: HasHeaders[Request] = new HasHeaders[Request] {
    def updateHeaders(r: Request)(f: Headers => Headers) = r.copy(headers = f(r.headers))
    def headers(r: Request) = r.headers
  }
}
