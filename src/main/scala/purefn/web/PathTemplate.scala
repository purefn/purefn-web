package purefn.web

import scalaz._, Scalaz._

import Route._

import java.net.URLEncoder.encode

object PathTemplate extends PathTemplates

sealed trait PathComponent[A] {
  def apply(a: A): String
  
  def fold[Z](lit: String => Z, param: (String, String => Option[A], A => String) => Z, remaining: => Z): Z  
}

sealed trait ExactPathComponent[A] extends PathComponent[A] {
  private[web] def routeTo[F[_], A](rt: Route[F, A]): Route[F, A]
}

final class PathParamComponent[A](name: String)(implicit read: Read[A], show: Show[A]) extends ExactPathComponent[A] {
  def apply(a: A) = encode(a.shows, "UTF-8")
  def routeTo[F[_], A](rt: Route[F, A]) = paramRoute(name, read.read(_).isDefined, rt, Route.noRoute)

  def fold[Z](lit: String => Z, param: (String, String => Option[A], A => String) => Z, remaining: => Z) = 
    param(name, read.read, show.shows)
}

final class PathLitComponent(val s: String) extends ExactPathComponent[Unit] {
  def apply(a: Unit) = encode(s, "UTF-8")
  def routeTo[F[_], A](rt: Route[F, A]) = litRoute(Map(s -> rt), Route.noRoute)

  def fold[Z](lit: String => Z, param: (String, String => Option[Unit], Unit => String) => Z, remaining: => Z) =
    lit(s)
}

object ** extends PathComponent[List[String]] {
  override def apply(a: List[String]): String = a.map(encode(_, "UTF-8")).intersperse("/").suml

  def fold[Z](lit: String => Z, param: (String, String => Option[List[String]], List[String] => String) => Z, remaining: => Z) =
    remaining
}

sealed trait PathTemplate[Params <: HList] { self =>
  def apply(p: Params): String

  private[web] def routeTo[F[_]: MonadWeb, A](f: F[A]): Route[F, A]
  
  def /:[T](pc: ExactPathComponent[T]): PathTemplate[HCons[T, Params]] = new PathTemplate[HCons[T, Params]] {
    def apply(params: HCons[T, Params]): String = pc(params.head) + "/" + self(params.tail)
    
    def routeTo[F[_]: MonadWeb, A](f: F[A]) = pc.routeTo(self.routeTo(f))
  }
}

sealed trait HList

case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  def ::[T](v: T) = HCons(v, this)
}

object HNil extends HList {
  def ::[T](v: T) = HCons(v, this)
}

sealed trait Read[A] {
  def read(s: String): Option[A]
}

object Read extends Reads

trait Reads {
  def reads[A](f: String => Option[A]): Read[A] = new Read[A] {
    def read(s: String) = f(s)
  }
  
  implicit def StringRead: Read[String] = reads[String](Some(_))
  
  implicit def IntRead: Read[Int] = reads[Int](s => 
    try { 
      Some(Integer.parseInt(s))
    } catch {
      case e: NumberFormatException => None
    })
  
  implicit def BooleanRead: Read[Boolean] = reads[Boolean](s =>
    if (CaseInsensitive(s) === CaseInsensitive("true")) Some(true)
    else if (CaseInsensitive(s) === CaseInsensitive("true")) Some(false)
    else None
  )
}

trait PathTemplates {
  def pathLit(s: String): PathLitComponent = new PathLitComponent(s)
  
  def pathParam[A: Read : Show](name: String): PathParamComponent[A] = new PathParamComponent[A](name)
  
  implicit def exactPathComponent2PathTemplate[T](pc: ExactPathComponent[T]): PathTemplate[HCons[T,HNil.type]] = new PathTemplate[HCons[T,HNil.type]] {
    def apply(params: HCons[T,HNil.type]) = pc(params.head)
    
    private def exactPath[F[_], A](f: F[A])(implicit mw:  MonadWeb[F]): F[A] = {
      import Web.{getRequest, pass}
      implicit val b = mw.bind
      getRequest flatMap (r => if (r.pathInfo == "") f else pass)
    }
    
    def routeTo[F[_]: MonadWeb, A](f: F[A]) = pc.routeTo(resourceRoute(exactPath(f)))
  }
  
  implicit def remainingPathComponent2PathTemplate(pc: **.type): PathTemplate[HCons[List[String],HNil.type]] = new PathTemplate[HCons[List[String], HNil.type]] {
    def apply(params: HCons[List[String],HNil.type]) = pc(params.head)

    def routeTo[F[_]: MonadWeb, A](f: F[A]) = resourceRoute(f)
  }
}
