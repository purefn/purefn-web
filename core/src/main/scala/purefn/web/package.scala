package purefn

import scalaz._
import effect.IO
import iteratee.{EnumeratorT, IterateeT}

package object web {
  type Headers = Map[CaseInsensitive[String], NonEmptyList[String]]

  type WebResult[A] = Option[Validation[Response, A]]
  type WebIter[A] = IterateeT[Throwable, String, IO, A]
  type WebIterState[S, A] = StateT[WebIter, S, A]
  type WebState[A] = WebIterState[WebData, A]
  type WebFn[S, A] = Kleisli[Web, S, A]
  type Params = Map[String, List[String]]

  type ResponseBody = EnumeratorT[Throwable, String, IO]
  
  object WebFn extends WebFnInstances
  object Headers extends HeadersFunctions
}