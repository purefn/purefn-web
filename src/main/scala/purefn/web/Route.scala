package purefn.web

import scalaz._, Scalaz._

import PathTemplate._, Web.{getRequest, putRequest, modifyRequest, pass, Params}

import java.net.URLDecoder

private[web] sealed trait Route[F[_], A] {
  def apply[Z](
      resource: F[A] => Z, 
      param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z, 
      lit: (Map[String, Route[F, A]], Route[F, A]) => Z, 
      none: => Z): Z
}

object Route extends Routes

trait Routes {
  def route[F[_], A](routes: (PathTemplate[_], F[A])*)(implicit mws: MonadWeb[F]): F[A] = {
    implicit val ftr = mws.functor
    implicit val bd = mws.bind
    lazy val router = (routes map (rt => rt._1.routeTo(rt._2))).suml
    for {
      req <- getRequest[F]
      val p = req.pathInfo
      a <- routeReq(router, Nil, splitPath(p), Map())
    } yield a
  }

  private def routeReq[F[_]: MonadWeb, A](router: Route[F, A], ctx: List[String], path: List[String], params: Params): F[A] = {
    implicit val plus = implicitly[MonadWeb[F]].plus
    def updateContextPath(ctx: List[String], r: Request): Request = {
      val n = ctx.intersperse("/").suml.length
      if (n == 0) r
      else r.copy(
        pathInfo = r.pathInfo.drop(n+1),
        contextPath = Seq(r.contextPath, r.pathInfo.take(n), "/").suml)
    }
    router(
      none = pass,
      lit = (rs, fb) => path match {
        case Nil => routeReq(fb, ctx, Nil, params)
        case cwd :: rest => 
          rs.get(cwd).
            map(routeReq(_, cwd :: ctx, rest, params) <+> routeReq(fb, ctx, path, params)).
            getOrElse(routeReq(fb, ctx, path, params))
      },
      param = (name, matches, route, fb) => path match {
        case Nil => routeReq(fb, ctx, Nil, params)
        case cwd :: rest =>
          matches(cwd) option routeReq(route, cwd::ctx, rest, params |+| Map(name -> Seq(cwd))) getOrElse pass <+>
            routeReq(fb, ctx, path, params)
      },
      resource = handler => withModifiedRequest(handler)(req => 
        updateContextPath(ctx, req.copy(params = params |+| req.params))
      )
    )
  }
  
  def withModifiedRequest[F[_], A](w: F[A])(f: Request => Request)(implicit mw: MonadWeb[F]): F[A] = {
    implicit val bind = mw.bind
    implicit val plus = mw.plus
    implicit val ftr = mw.functor
    def tryReq(req: Request): F[A] = 
      for {
        _ <- modifyRequest(f)
        result <- w
        _ <- putRequest(req)
      } yield result
    getRequest flatMap (req => tryReq(req) <+> (putRequest(req) >|> pass))
  }
  
  private def splitPath(p: String): List[String] = p.split("/").map(URLDecoder.decode(_, "UTF-8")).toList

  private[web] def litRoute[F[_], A](rs: Map[String, Route[F, A]], alt: Route[F, A]): Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z, 
        param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z, 
        lit: (Map[String, Route[F, A]], Route[F, A]) => Z, 
        none: => Z) = lit(rs, alt)
  }

  private[web] def paramRoute[F[_], A](name: String, matches: String => Boolean, r: Route[F, A], fb: Route[F, A]): Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z, 
        param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z, 
        lit: (Map[String, Route[F, A]], Route[F, A]) => Z, 
        none: => Z) = param(name, matches, r, fb)
  }

  private[web] def resourceRoute[F[_], A](f: F[A]): Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z, 
        param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z, 
        lit: (Map[String, Route[F, A]], Route[F, A]) => Z, 
        none: => Z) = resource(f)
  }

  private[web] def noRoute[F[_], A]: Route[F, A] = new Route[F, A] {
    def apply[Z](
        resource: F[A] => Z, 
        param: (String, String => Boolean, Route[F, A], Route[F, A]) => Z, 
        lit: (Map[String, Route[F, A]], Route[F, A]) => Z, 
        none: => Z) = none
  }

  private implicit def RouteZero[F[_], A]: Zero[Route[F, A]] = new Zero[Route[F, A]] {
    val zero: Route[F, A] = noRoute
  }

  private implicit def RouteSemigroup[F[_], A](implicit mw: MonadWeb[F]): Semigroup[Route[F, A]] = {
    implicit val alt = mw.plus
    semigroup(r1 => r2 =>
      r1(
        none = r2,
        resource = a => r2.apply(
          none = r1,
          resource = b => resourceRoute(a <+> b),
          lit = (_, _) => litRoute(Map(), r1) |+| r2,
          param = (name, matches,  rt, fb) => paramRoute(name, matches, rt, fb |+| r1)
        ),
        param = (name1, matches1, rt1, fb1) => r2.apply(
          none = r1,
          resource = _ => paramRoute(name1, matches1, rt1, fb1 |+| r2),
          lit = (rs, fb2) => litRoute(rs, fb2 |+| r1),
          param = (name2, matches2, rt2, fb2) => {
            lazy val rh1 = routeHeight(r1)
            lazy val rh2 = routeHeight(r2)
            lazy val np1 = routeEarliestNonParam(rt1, 1)
            lazy val np2 = routeEarliestNonParam(rt2, 1)
            if (rh1 > rh2) paramRoute(name1, matches1, rt1, fb1 |+| r2)
            else if (rh1 < rh2) paramRoute(name2, matches2, rt2, fb2 |+| r1)
            else if (np1 > np2) paramRoute(name2, matches2, rt2, fb2 |+| r1)
            else paramRoute(name1, matches1, rt1, fb1 |+| r2)
          }
        ),
        lit = (rs1, fb1) => r2.apply(
          none = r1,
          resource = _ => litRoute(rs1, fb1 |+| r2),
          param = (_, _, _, _) => litRoute(rs1, fb1 |+| r2),
          lit = (rs2, fb2) => litRoute(rs1 |+| rs2, fb1 |+| fb2)
        )
      )
    )
  }
  
  private def routeHeight[F[_], A](r: Route[F, A]): Int = r(
    none = 1,
    resource = _ => 1,
    param = (_, _, rt, _) => 1 + routeHeight(rt),
    lit = (rs, _) => 1 + rs.values.map(routeHeight).foldLeft(1)(math.max)
  )
    
  private def routeEarliestNonParam[F[_], A](r: Route[F, A], n: Int): Int = r(
    none = n,
    resource = _ => n,
    lit = (_, _) => n,
    param = (_, _, rt, _) => routeEarliestNonParam(rt, n + 1)
  )
  
  private implicit def RouteMonoid[F[_], A](implicit sg: MonadWeb[F]): Monoid[Route[F, A]] = monoid
}
