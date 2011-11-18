package purefn.web

import scalaz._, Scalaz._, scalaz.iteratee._, effect._

sealed case class WebData(request: Request, response: Response)

sealed trait Web[A] {
  import Web._
  val state: WebState[WebResult[A]]
  
  def map[B](f: A => B): Web[B] = {
    Web(state map(_ map (_ map f)))
  }
  
  def flatMap[B](f: A => Web[B]): Web[B] = Web(state flatMap(
    _ fold (
      none = none.point[WebState],
      some = _.fold(
        failure = r => r.fail.some.point[WebState],
        success = a => f(a).state))))

  def run[A](req: Request): WebIter[(Request, Response)] = {
    state.runT(WebData(req, emptyResponse.copy(httpVersion = req.httpVersion))) map (rwd => 
      (rwd._2.request, rwd._1.map ( _.fail | rwd._2.response) getOrElse (fourOhFour)))
  }
}

object Web extends Webs with
  MonadWebs with
  Routes with
  Requests with
  Responses with
  Headerss with
  PathTemplates {
  
  def apply[A](s: WebState[WebResult[A]]): Web[A] = new Web[A] {
    val state = s
  }
}

trait Webs {
  import Response._
  type WebResult[A] = Option[Validation[Response, A]]
  type WebIter[A] = IterateeT[Throwable, String, IO, A]
  type WebState[A] = StateT[WebData, WebIter, A]
  type WebFn[S, A] = ReaderT[S, Web, A]
  type Params = Map[String, Seq[String]]
  
  implicit def WebPointed: Pointed[Web] = new Pointed[Web] {
    def point[A](a: => A) = Web(a.success.some.point[WebState])
  }

  implicit def WebBind: Bind[Web] = new Bind[Web] {
    def bind[A, B](f: A => Web[B]) = _ flatMap f
  }

  implicit def WebFunctor: Functor[Web] = new Functor[Web] {
    def fmap[A, B](f: A => B) = _ map f
  }
  
  implicit def WebMonad: Monad[Web] = monadBP

  implicit def WebPlus: Plus[Web] = new Plus[Web] {
    def plus[A](a: Web[A], b: => Web[A]) = 
      Web(a.state flatMap (r => r map(_ => r.point[WebState]) getOrElse(b.state)))
  }
  
  def pass[F[_], A](implicit mws: MonadWeb[F]): F[A] = 
    mws.lift(Web(stateT[WebData, WebIter, WebResult[A]](wd => (none[Validation[Response, A]], wd).point[WebIter])))
  
  def finishWith[F[_], A](r: Response)(implicit mws: MonadWeb[F]): F[A] =
    mws.lift(Web(r.fail.some.point[WebState]))

  /* Web versions of get, modify, and put */
  def wget: Web[WebData] = Web(getT[WebData, WebIter].map(wd => wd.success.some))

  def wput(wd: => WebData): Web[Unit] =
    Web(putT[WebData, WebIter](wd) flatMap (_ => ().success.some.point[WebState]))
  
  def wmodify(f: WebData => WebData): Web[Unit] = 
    Web(modifyT[WebData, WebIter](f) flatMap (_ => ().success.some.point[WebState]))

  /* Request handling functions */
  def getRequest[F[_]](implicit mw: MonadWeb[F]): F[Request] = mw.lift(wget map (_.request))

  def putRequest[F[_]](request: Request)(implicit mws: MonadWeb[F]): F[Unit] = 
    mws.lift(wmodify(_.copy(request = request)))

  def modifyRequest[F[_]](f: Request => Request)(implicit mws: MonadWeb[F]): F[Unit] = 
    mws.lift(wmodify(ws => ws.copy(request = f(ws.request))))

  /* Response handling functions */
  def getResponse[F[_]](implicit mws: MonadWeb[F]): F[Response] = mws.lift(wget map (_.response))

  def putResponse[F[_]](response: Response)(implicit mws: MonadWeb[F]): F[Unit] = 
    mws.lift(wmodify(_.copy(response = response)))

  def modifyResponse[F[_]](f: Response => Response)(implicit mws: MonadWeb[F]): F[Unit] = 
    mws.lift(wmodify(ws => ws.copy(response = f(ws.response))))
    
  /* Writing responses */
  def writeStr[F[_]: MonadWeb](s: String): F[Unit] = addToBody(new Forall[ResponseEnumT] { 
    def apply[A] = enumStream(Stream(s))
  })
  
  def addToBody[F[_]](enum: ResponseBody)(implicit mw: MonadWeb[F]): F[Unit] =
    modifyResponse(modifyResponseBody(new (ResponseEnumT ~> ResponseEnumT) {
      def apply[A](e: ResponseEnumT[A]) = e |+| enum[A]
    }))
}
