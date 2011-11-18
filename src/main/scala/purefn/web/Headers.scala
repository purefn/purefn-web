package purefn.web

trait HeadersFunctions {
  import scalaz._, NonEmptyList.nels, std.map._, syntax.monoid._
  
  def addHeader[A](k: CaseInsensitive[String], v: String)(a: A)(implicit h: HasHeaders[A]): A = 
    h.updateHeaders(a)(_ |+| Map(k -> nels(v)))
    
  def setHeader[A](k: CaseInsensitive[String], v: String)(a: A)(implicit h: HasHeaders[A]): A =
    h.updateHeaders(a)(_ + (k -> nels(v)))
  
  def getHeaders[A](k: CaseInsensitive[String])(a: A)(implicit h: HasHeaders[A]): Option[NonEmptyList[String]] = 
    h.headers(a).get(k)
  
  def deleteHeader[A](k: CaseInsensitive[String])(a: A)(implicit h: HasHeaders[A]): A = 
    h.updateHeaders(a)(_ - k)
    
  def showHeaders[A](a: A)(implicit h: HasHeaders[A]): List[String] = {
    import std.string._, syntax.show._, syntax.foldable._, std.list._ 
    h.headers(a).toList.map(kv => List(kv._1.original.shows, ": ", kv._2.shows).foldMap())
  }
}
