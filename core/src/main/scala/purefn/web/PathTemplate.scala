package purefn.web

import scalaz._, Scalaz._, typelevel._, Typelevel.{:: => _, _}

import Route._

import java.net.URLEncoder.encode

sealed trait PathTemplate[Params <: HList] { 
  self =>
  
  import PathTemplate._
    
  def toURI(p: Params): String // TODO create a decent, immutable URI type
  
  private[web] def routeTo[F[_]: MonadWeb, A](f: F[A]): Route[F, A]
  
  def /:(lit: String): PathTemplate[Params] = new PathTemplate[Params] {
    def toURI(params: Params): String = encode(lit, "UTF-8") + (if (self != PNil) "/" + self.toURI(params) else "")
    def routeTo[F[_]: MonadWeb, A](f: F[A]) = litRoute(Map(lit -> self.routeTo(f)), noRoute)
  }
  
  def /:[A: Read: Show](p: Param[A]): PathTemplate[HCons[_ <: A, Params]] = new PathTemplate[HCons[_ <: A, Params]] {
    def toURI(params: HCons[_ <: A, Params]): String = 
      encode(params.head.shows, "UTF-8") + (if (self != PNil) "/" + self.toURI(params.tail) else "")
    
    def routeTo[F[_]: MonadWeb, B](f: F[B]) = paramRoute(p.name, Read[A].read(_).isDefined, self.routeTo(f), noRoute)
  }
}

object PathTemplate extends PathTemplateFunctions

trait PathTemplateFunctions {
  
  sealed case class Param[A](name: String)
  
  def $: PathTemplate[HNil] = PNil
    
  def ** : PathTemplate[HCons[List[String], HNil]] = new PathTemplate[HCons[List[String], HNil]] {
    def toURI(params: HCons[List[String], HNil]) = params.head map (encode(_, "UTF-8")) mkString("/")
    def routeTo[F[_]: MonadWeb, A](f: F[A]) = resourceRoute(f)
  }

  private[web] case object PNil extends PathTemplate[HNil] {
    def toURI(params: HNil) = ""
    def routeTo[F[_]: MonadWeb, A](f: F[A]) = resourceRoute(exactPath(f))
     
    import Web.{getRequest, pass}
    private def exactPath[F[_], A](f: F[A])(implicit mw:  MonadWeb[F]): F[A] = 
      getRequest flatMap (r => if (r.pathInfo == "") f else pass)    
  }
}
