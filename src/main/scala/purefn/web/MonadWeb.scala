package purefn.web

import scalaz._

trait MonadWeb[F[_]] extends MonadPlus[F] {
  def liftWeb[A](wa: Web[A]): F[A]
}

object MonadWeb {
  @inline def apply[F[_]](implicit F: MonadWeb[F]): MonadWeb[F] = F
}
