package purefn.web

import scalaz._

sealed trait MonadWeb[F[_]] {
  def lift[A]: Web[A] => F[A]
  val monad: Monad[F]
  val plus: Plus[F]
  
  def bind: Bind[F] = new Bind[F] {
    def bind[A, B](f: A => F[B]): F[A] => F[B] =
      monad.bd(f)
  }

  def pointed: Pointed[F] = new Pointed[F] {
    def point[A](a: => A): F[A] =
      monad.point(a)
  }

  def functor: Functor[F] = new Functor[F] {
    def fmap[A, B](f: A => B): F[A] => F[B] =
      monad.fmap(f)
  }

  def pointedFunctor: PointedFunctor[F] =
    monad.pointedFunctor

  def applic: Applic[F] =
    monad.applic

  def applicative: Applicative[F] =
    monad.applicative

  def fmap[A, B](f: A => B): F[A] => F[B] =
    monad.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    monad.apply(f)

  def bd[A, B](f: A => F[B]): F[A] => F[B] =
    monad.bd(f)

  def jn[A]: F[F[A]] => F[A] =
    monad.jn[A]

  def point[A](a: => A): F[A] =
    monad.point(a)
}

trait MonadWebs {
  implicit def WebMonadWeb: MonadWeb[Web] = new MonadWeb[Web] {
    def lift[A] = identity
    val monad = implicitly[Monad[Web]]
    val plus = implicitly[Plus[Web]]
  }
  
  import Web._
  implicit def WebFnMonadWeb[S]: MonadWeb[({type λ[α] = WebFn[S, α]})#λ] = new MonadWeb[({type λ[α] = WebFn[S, α]})#λ] {
    def lift[A] = a => Kleisli(s => a)
    val monad = implicitly[Monad[({type λ[α] = WebFn[S, α]})#λ]]
    val plus = implicitly[Plus[({type λ[α] = WebFn[S, α]})#λ]]
  }
}

object MonadWeb extends MonadWebs
