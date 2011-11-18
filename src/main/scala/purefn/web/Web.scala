package purefn.web

import scalaz._
import std.option._, optionSyntax._
import syntax.pointed._
import syntax.validation._

sealed case class WebData(request: Request, response: Response)

sealed trait Web[A] {
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
    import Response._
    state(WebData(req, emptyResponse.copy(httpVersion = req.httpVersion))) map (rwd => 
      (rwd._2.request, rwd._1.map ( _.fail | rwd._2.response) getOrElse (fourOhFour)))
  }
}

object Web extends WebFunctions with WebInstances with WebFnInstances {
  def apply[A](s: WebState[WebResult[A]]): Web[A] = new Web[A] {
    val state = s
  }
}

trait WebInstances {
  implicit def webInstances: MonadWeb[Web] = new MonadWeb[Web] {
    def empty[A] = Web(none[Validation[Response, A]].point[WebState])
    def point[A](a: => A) = Web(a.success.some.point[WebState])
    def bind[A, B](fa: Web[A])(f: A => Web[B]) = fa flatMap f
    override def map[A, B](fa: Web[A])(f: A => B) = fa map f
    def plus[A](a: Web[A], b: => Web[A]) = 
      Web(a.state flatMap (r => r map(_ => r.point[WebState]) getOrElse(b.state)))
    def liftWeb[A](wa: Web[A]) = wa
  }  
}

trait WebFunctions {
  def pass[F[_]: MonadWeb, A]: F[A] = MonadWeb[F].empty[A]
  
  def finishWith[F[_]: MonadWeb, A](r: Response): F[A] =
    MonadWeb[F].liftWeb(Web(r.fail.some.point[WebState]))

  /* Web versions of init, modify, and put */

  def winit: Web[WebData] = 
    Web(MonadState[WebIterState, WebData].init.map(wd => wd.success.some))

  def wput(wd: => WebData): Web[Unit] =
    Web(MonadState[WebIterState, WebData].put(wd) flatMap (_ => ().success.some.point[WebState]))
  
  def wmodify(f: WebData => WebData): Web[Unit] = 
    Web(MonadState[WebIterState, WebData].modify(f) flatMap (_ => ().success.some.point[WebState]))

  /* Request handling functions */
  def getRequest[F[_]: MonadWeb]: F[Request] = MonadWeb[F].liftWeb(winit map (_.request))

  def putRequest[F[_]: MonadWeb](request: Request): F[Unit] = MonadWeb[F].liftWeb(wmodify(_.copy(request = request)))

  def modifyRequest[F[_]: MonadWeb](f: Request => Request): F[Unit] = 
  	MonadWeb[F].liftWeb(wmodify(ws => ws.copy(request = f(ws.request))))

  /* Response handling functions */
  def getResponse[F[_]: MonadWeb]: F[Response] = MonadWeb[F].liftWeb(winit map (_.response))

  def putResponse[F[_]: MonadWeb](response: Response): F[Unit] = MonadWeb[F].liftWeb(wmodify(_.copy(response = response)))

  def modifyResponse[F[_]: MonadWeb](f: Response => Response): F[Unit] = 
    MonadWeb[F].liftWeb(wmodify(ws => ws.copy(response = f(ws.response))))
    
  /* Writing responses */
  import iteratee.EnumeratorT.enumStream

  def writeStr[F[_]: MonadWeb](s: String): F[Unit] = addToBody(new Forall[ResponseEnumT] { 
    def apply[A] = enumStream(Stream(s))
  })
  
  import Response._
  import syntax.monoid._
  import iteratee._
  
  def addToBody[F[_]: MonadWeb](enum: ResponseBody): F[Unit] =
    modifyResponse(modifyResponseBody(new (ResponseEnumT ~> ResponseEnumT) {
      def apply[A](e: ResponseEnumT[A]) = {
        e |+| enum[A]
      }
    }))
}
