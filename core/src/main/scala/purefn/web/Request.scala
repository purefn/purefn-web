package purefn.web

sealed case class Request(
    httpVersion: String = "1.1"
  , method: String = "GET"
  , pathInfo: String = ""
  , headers: Headers = Map()
  , contextPath: String = ""
  , params: Params = Map()
  )

object Request extends RequestInstances {
  sealed abstract class Method

  object Method {
    case object GET extends Method
    case object HEAD extends Method
    case object POST extends Method
    case object PUT extends Method
    case object DELETE extends Method
    case object TRACE extends Method
    case object OPTIONS extends Method
    case object CONNECT extends Method
  }
}

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
      def method = List("method: " + r.method)
      def version = List("version: " + r.httpVersion)
      def pathInfo = List("path info: " + r.pathInfo)
      def contextPath = List("context path: " + r.contextPath)
      def params = List(
        List("params: ", "=============================="),
        r.params.toList.flatMap(kv => List(kv._1.shows, ": ", kv._2.toList.shows)),
        List("==============================")).flatten
      def body: String =
        List(hdrs, method, version, pathInfo, contextPath, params).flatMap(_.map(("    " + _) andThen (_ + "\n"))).foldMap()     
      List("Request <\n", body, ">").foldMap()
    }
  }

  implicit def RequestHasHeaders: HasHeaders[Request] = new HasHeaders[Request] {
    def updateHeaders(f: Headers => Headers)(r: Request) = r.copy(headers = f(r.headers))
    def headers(r: Request) = r.headers
  }
}
