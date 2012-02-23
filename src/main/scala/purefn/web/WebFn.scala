package purefn.web

import scalaz._

// mixed into the Web object so they are implicitly found
trait WebFnInstances {
  implicit def webFnMonadWeb[S]: MonadWeb[({type λ[α] = WebFn[S, α]})#λ] = new MonadWeb[({type λ[α] = WebFn[S, α]})#λ] {
    def empty[A] = Kleisli(_ => PlusEmpty[Web].empty)
    def point[A](a: => A) = Kleisli(_ => Pointed[Web].point(a))
    def bind[A, B](fa: WebFn[S, A])(f: A => WebFn[S, B]) = fa flatMap f
    override def map[A, B](fa: WebFn[S, A])(f: A => B) = fa map f
    def plus[A](a: WebFn[S, A], b: => WebFn[S, A]) = Kleisli(s => Plus[Web].plus(a(s), b(s)))
    def liftWeb[A](wa: Web[A]) = Kleisli(s => wa)
  }
}
