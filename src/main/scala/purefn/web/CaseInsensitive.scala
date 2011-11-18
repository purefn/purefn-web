package purefn.web

import scalaz._
import syntax.equal._

sealed trait CaseInsensitive[A] {
  val original: A
  def foldedCase: A
}

object CaseInsensitive extends CaseInsensitiveInstances {
  def apply[A](a: A)(implicit fc: FoldCase[A], eq: Equal[A]): CaseInsensitive[A] = mk(a, fc.foldCase(a))
  
  private[web] def mk[A: Equal](a: A, fc: => A): CaseInsensitive[A] = new CaseInsensitive[A] {
    val original = a
    lazy val foldedCase = fc
    
    override def equals(other: Any): Boolean = other match {
      case that: CaseInsensitive[A] => foldedCase === that.foldedCase
      case _ => false
    }
    
    override lazy val hashCode: Int = 37 * (41 + foldedCase.hashCode)
  }
}

trait CaseInsensitiveInstances {
  implicit def CaseInsensitiveMonoid[A: FoldCase : Monoid : Equal]: Monoid[CaseInsensitive[A]] =
    new Monoid[CaseInsensitive[A]] {
      def zero = CaseInsensitive[A](Monoid[A].zero)
      def append(a: CaseInsensitive[A], b: => CaseInsensitive[A]) = 
        CaseInsensitive(Semigroup[A].append(a.original, b.original))
    }
  
  implicit def CaseInsensitiveEqual[A: Equal]: Equal[CaseInsensitive[A]] = 
    new Equal[CaseInsensitive[A]] {
      def equal(a: CaseInsensitive[A], b: CaseInsensitive[A]) = 
        Equal[A].equal(a.foldedCase, b.foldedCase)
    }
  
  implicit def CaseInsensitiveOrder[A: Order]: Order[CaseInsensitive[A]] = 
    new Order[CaseInsensitive[A]] {
      def order(a: CaseInsensitive[A], b: CaseInsensitive[A]) = Order[A].order(a.foldedCase, b.foldedCase)
    }
  
  implicit def CaseInsensitiveShow[A: Show]: Show[CaseInsensitive[A]] =
    new Show[CaseInsensitive[A]] {
      def show(a: CaseInsensitive[A]) = Show[A].show(a.original)
    }
}

trait FoldCase[A] {
  def foldCase(a: A): A
}

object FoldCase extends FoldCaseInstances

trait FoldCaseInstances {
  implicit def StringFoldCase: FoldCase[String] = new FoldCase[String] {
    def foldCase(s: String) = s.toLowerCase
  }
}
