package purefn.web
package apparatus

import purefn.web.{CaseInsensitive => CI} 
import Response.{fourOhSix, modifyResponseBody}
import Headers.{getHeaders, setHeader}

import scalaz._, effect._, scalaz.iteratee._
import NonEmptyList._, Validation._
import syntax.equal._
import syntax.monoid._
import syntax.pointed._
import syntax.traverse._
import syntax.std.optionV._
import std.option._
import std.string._

object WebResource extends WebResourceFunctions {
  def apply[S, C](
      init: InitFn[S, C] = initIdent[S],
      contentTypesProvided: ContentTypesProvidedFn[C]): WebFn[S, Unit] = webResource(init, contentTypesProvided)
}

trait WebResourceFunctions {
  implicit def provideContentTypes[C](ts: NonEmptyList[(String, BodyProducingFn[C])]): ContentTypesProvidedFn[C] = 
    StateT(s => (ts, s).point[WebResourceIter])

  implicit def provideContentType[C](t: (String, BodyProducingFn[C])): ContentTypesProvidedFn[C] =
    provideContentTypes(nels(t))

  def initIdent[S]: InitFn[S, S] = Kleisli(s => s.point[WebState])
    
  def webResource[S, C](
      init: InitFn[S, C] = initIdent[S],
      contentTypesProvided: ContentTypesProvidedFn[C]): WebFn[S, Unit] = {

    type WebResourceFnC[A] = WebResourceFn[C, A]

    def pickBodyProducingFn(cts: NonEmptyList[(String, BodyProducingFn[C])]): 
        WebResourceFnC[Validation[Response, (String, BodyProducingFn[C])]] = {
      def findBestMatch(accepts: NonEmptyList[String]): Validation[Response, (String, BodyProducingFn[C])] = 
        // TODO some algorithm to parse accepts and find the best type - for now, just use text/html
        cts.list.find(ct => ct._1 === "text/html").toSuccess(fourOhSix)
        
      getRequest[C] map 
        getHeaders[Request](CI("Accept")) map 
        (_ map (findBestMatch) getOrElse (success(cts.head)))
    }
    
    def runBodyProducingFn(fn: (String, BodyProducingFn[C])): WebResourceFnC[Unit] = fn._2 flatMap (enum =>
      modifyResponse(modifyResponseBody(_ |+| enum) andThen setHeader[Response](CI("Content-Type"), fn._1)))
    
    def addBodyToResponse(b: ResponseBody): WebResourceFnC[Unit] = modifyResponse(modifyResponseBody(_ |+| b))

    def stateMachine: WebResourceFnC[Validation[Response, Unit]] =
      for {
        contentTypes <- contentTypesProvided
        bodyFn       <- pickBodyProducingFn(contentTypes)
        result       <- (bodyFn map runBodyProducingFn).traverse(identity) // couldn't get sequence working here
      } yield result
      
    def dropContext(r: (Validation[Response, Unit], (C, WebData))) = (some(r._1), r._2._2)

    Kleisli(s => Web(StateT[WebIter, WebData, Option[Validation[Response, Unit]]](wd => 
      init(s) apply (wd) flatMap(s => 
        stateMachine apply (s) map dropContext))))
  }
  
  /* WebResourceFn versions of get, modify, and put */
  def init[C]: WebResourceFn[C, (C, WebData)] = MonadState[WebResourceIterState, (C, WebData)].init

  def put[C](cwd: => (C, WebData)): WebResourceFn[C, Unit] = MonadState[WebResourceIterState, (C, WebData)].put(cwd)
  
  def modify[C](f: (C, WebData) => (C, WebData)): WebResourceFn[C, Unit] =
    MonadState[WebResourceIterState, (C, WebData)].modify(cw => f(cw._1, cw._2))

  /* Request handling functions */
  def getRequest[C]: WebResourceFn[C, Request] = init map (_._2.request)

  def putRequest[C](request: Request): WebResourceFn[C, Unit] = modify((c, w) => (c, w.copy(request = request)))

  def modifyRequest[C](f: Request => Request): WebResourceFn[C, Unit] = modify((c, w) => (c, w.copy(request = f(w.request))))

  /* Response handling functions */
  def getResponse[C]: WebResourceFn[C, Response] = init map (_._2.response)

  def putResponse[C](response: Response): WebResourceFn[C, Unit] = modify((c, w) => (c, w.copy(response = response)))

  def modifyResponse[C](f: Response => Response): WebResourceFn[C, Unit] = 
    modify((c, w) => (c, w.copy(response = f(w.response))))
    
  /* Request context handling functions */
  def getContext[C]: WebResourceFn[C, C] = init map(_._1)
  
  def putContext[C](c: C): WebResourceFn[C, Unit] = modify((_, w) => (c, w))
  
  def modifyContext[C](f: C => C): WebResourceFn[C, Unit] = modify((c, w) => (f(c), w))
}
