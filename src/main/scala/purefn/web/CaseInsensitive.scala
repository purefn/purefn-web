package purefn.web

import scalaz._, Scalaz._

sealed trait CaseInsensitive[A] {
  val original: A
  def foldedCase: A
}

object CaseInsensitive {
  def apply[A](a: A)(implicit fc: FoldCase[A], eq: Equal[A]): CaseInsensitive[A] = mk(a, fc.foldCase(a))
  
  private def mk[A: Equal](a: A, fc: => A): CaseInsensitive[A] = new CaseInsensitive[A] {
    val original = a
    lazy val foldedCase = fc
    
    override def equals(other: Any): Boolean = other match {
      case that: CaseInsensitive[A] => foldedCase === that.foldedCase
      case _ => false
    }
    
    override lazy val hashCode: Int = 37 * (41 + foldedCase.hashCode)
  }
  
  implicit def CaseInsensitiveZero[A](implicit fc: FoldCase[A], z: Zero[A], eq: Equal[A]): Zero[CaseInsensitive[A]] = 
    Zero.zero(CaseInsensitive(z.zero))
    
  implicit def CaseInsensitiveSemigroup[A](implicit fc: FoldCase[A], sg: Semigroup[A], eq: Equal[A]): Semigroup[CaseInsensitive[A]] = 
    Semigroup.semigroup(a => b => mk(sg.append(a.original, b.original), sg.append(a.foldedCase, b.foldedCase)))
  
  implicit def CaseInsensitiveMonoid[A: FoldCase : Semigroup : Zero : Equal]: Monoid[CaseInsensitive[A]] = Monoid.monoid
  
  implicit def CaseInsensitiveEqual[A](implicit eq: Equal[A]): Equal[CaseInsensitive[A]] = 
    Equal.equal(a => b => eq.equal(a.foldedCase)(b.foldedCase))
  
  implicit def CaseInsensitiveOrder[A](implicit ord: Order[A]): Order[CaseInsensitive[A]] = 
    Order.order(a => b => ord.order(a.foldedCase)(b.foldedCase))
  
  implicit def CaseInsensitiveString(s: String): CaseInsensitive[String] = CaseInsensitive(s)
  
  implicit def CaseInsensitiveShow[A](implicit s: Show[A]): Show[CaseInsensitive[A]] =
    Show.shows(a => s.shows(a.original))
}

trait FoldCase[A] {
  def foldCase(a: A): A
}

object FoldCase {
  implicit def StringFoldCase: FoldCase[String] = new FoldCase[String] {
    def foldCase(s: String) = s.toLowerCase
  }
}
