package purefn.web

import PathTemplate._, Web.{getRequest, putRequest, modifyRequest, pass}

import java.net.URLDecoder

private[web] sealed trait Route[F[_], A] {
  def apply[Z](
      resource: F[A] => Z
    , param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z
    , lit: (Map[String, Route[F, A]], Route[F, A]) => Z
    , none: => Z
    ): Z
}

object Route extends RouteFunctions with RouteInstances

import scalaz._
import syntax.monoid._
import syntax.plus._
import std.map._
trait RouteFunctions {
  import std.list._
  import std.string._
  import syntax.bind._
  import syntax.foldable._
  import syntax.std.listV._
  import syntax.std.booleanV._
  
  def route[F[_]: MonadWeb, A](routes: (PathTemplate[_], F[A])*): F[A] = {
    lazy val a = routes map (rt => rt._1.routeTo(rt._2))
    lazy val router = (a).toList.foldMap()
    for {
      req <- getRequest[F]
      val p = req.pathInfo
      a <- routeReq(router, Nil, splitPath(p), Map())
    } yield a
  }

  private def routeReq[F[_]: MonadWeb, A](router: Route[F, A], ctx: List[String], path: List[String], params: Params): F[A] = {
    def updateContextPath(ctx: List[String], r: Request): Request = {
      val n = ctx.intersperse("/").foldMap().length
      if (n == 0) r
      else r.copy(
          pathInfo = r.pathInfo.drop(n+1)
        , contextPath = List(r.contextPath, r.pathInfo.take(n), "/").foldMap()
        )
    }
    
    router(
        none = pass
      , lit = (rs, fb) => path match {
          case Nil => routeReq(fb, ctx, Nil, params)
          case cwd :: rest => 
            rs.get(cwd).
              map(routeReq(_, cwd :: ctx, rest, params) <+> routeReq(fb, ctx, path, params)).
              getOrElse(routeReq(fb, ctx, path, params))
        }
      , param = (name, matches, route, fb) => path match {
          case Nil => routeReq(fb, ctx, Nil, params)
          case cwd :: rest =>
            (matches(cwd) option routeReq(route, cwd::ctx, rest, params |+| Map(name -> List(cwd))) getOrElse pass) <+>
              routeReq[F, A](fb, ctx, path, params)
        }
      , resource = handler => withModifiedRequest(handler)(req => 
          updateContextPath(ctx, req.copy(params = params |+| req.params)))
      )
  }
  
  private def withModifiedRequest[F[_]: MonadWeb, A](w: F[A])(f: Request => Request): F[A] = {
    def tryReq(req: Request): F[A] = 
      for {
        _ <- modifyRequest(f)
        result <- w
        _ <- putRequest(req)
      } yield result
    getRequest flatMap (req => tryReq(req) <+> (putRequest(req) >> pass))
  }
  
  private def splitPath(p: String): List[String] = p.split("/").map(URLDecoder.decode(_, "UTF-8")).toList

  private[web] def litRoute[F[_], A](rs: Map[String, Route[F, A]], alt: Route[F, A]): Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z
      , param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z
      , lit: (Map[String, Route[F, A]], Route[F, A]) => Z
      , none: => Z
      ) = lit(rs, alt)
  }

  private[web] def paramRoute[F[_], A](name: String, matches: String => Boolean, r: Route[F, A], fb: Route[F, A]): Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z
      , param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z
      , lit: (Map[String, Route[F, A]], Route[F, A]) => Z
      , none: => Z
      ) = param(name, matches, r, fb)
  }

  private[web] def resourceRoute[F[_], A](f: F[A]): Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z
      , param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z
      , lit: (Map[String, Route[F, A]], Route[F, A]) => Z
      , none: => Z
      ) = resource(f)
  }

  private[web] def noRoute[F[_], A]: Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z
      , param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z
      , lit: (Map[String, Route[F, A]], Route[F, A]) => Z
      , none: => Z
      ) = none
  }
}

trait RouteInstances {
  implicit def routeMonoid[F[_]: MonadWeb, A]: Monoid[Route[F, A]] = new Monoid[Route[F, A]] {
    import Route.{litRoute, noRoute, paramRoute, resourceRoute}
    
    def zero = noRoute
    
    def append(r1: Route[F, A], r2: => Route[F, A]): Route[F, A] = r1( 
        none = r2
      , resource = a => r2.apply(
          none = r1
        , resource = b => resourceRoute(a <+> b)
        , lit = (_, _) => litRoute(Map(), r1) |+| r2
        , param = (name, matches,  rt, fb) => paramRoute(name, matches, rt, append(fb, r1))
        )
      , param = (name1, matches1, rt1, fb1) => r2.apply(
          none = r1
        , resource = _ => paramRoute(name1, matches1, rt1, append(fb1, r2))
        , lit = (rs, fb2) => litRoute(rs, append(fb2, r1))
        , param = (name2, matches2, rt2, fb2) => {
            lazy val rh1 = routeHeight(r1)
            lazy val rh2 = routeHeight(r2)
            lazy val np1 = routeEarliestNonParam(rt1, 1)
            lazy val np2 = routeEarliestNonParam(rt2, 1)
            if (rh1 > rh2) paramRoute(name1, matches1, rt1, append(fb1, r2))
            else if (rh1 < rh2) paramRoute(name2, matches2, rt2, append(fb2, r1))
            else if (np1 > np2) paramRoute(name2, matches2, rt2, append(fb2, r1))
            else paramRoute(name1, matches1, rt1, append(fb1, r2))
          }
      )
      , lit = (rs1, fb1) => r2.apply(
          none = r1
        , resource = _ => litRoute(rs1, append(fb1, r2))
        , param = (_, _, _, _) => litRoute(rs1, append(fb1, r2))
        , lit = (rs2, fb2) => litRoute(rs1 |+| rs2, append(fb1, fb2))
        )
      )
  }
    
  private def routeHeight[F[_], A](r: Route[F, A]): Int = r(
      none = 1
    , resource = _ => 1
    , param = (_, _, rt, _) => 1 + routeHeight(rt)
    , lit = (rs, _) => 1 + rs.values.map(routeHeight).foldLeft(1)(math.max)
    )
    
  private def routeEarliestNonParam[F[_], A](r: Route[F, A], n: Int): Int = r(
      none = n
    , resource = _ => n
    , lit = (_, _) => n
    , param = (_, _, rt, _) => routeEarliestNonParam(rt, n + 1)
    )
}
