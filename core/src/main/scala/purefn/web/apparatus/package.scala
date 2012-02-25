package purefn.web

import scalaz._, effect._, iteratee._

package object apparatus {
  type WebResourceIter[A] = IterateeT[Throwable, String, IO, A]
  type InitFn[S, C] = Kleisli[WebState, S, C]
  type WebResourceIterState[S, A] = StateT[WebResourceIter, S, A]
  type WebResourceFn[C, A] = WebResourceIterState[(C, WebData), A]
  type BodyProducingFn[C] = WebResourceFn[C, ResponseBody]
  type ContentTypesProvidedFn[C] = WebResourceFn[C, NonEmptyList[(String, BodyProducingFn[C])]]
}