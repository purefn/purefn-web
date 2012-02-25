package purefn.web

import scalaz.syntax.equal._
import scalaz.std.string._

sealed trait Read[A] {
  def read(s: String): Option[A]
}

object Read extends ReadInstances {
  @inline def apply[A: Read] = implicitly[Read[A]]
}

trait ReadInstances {
  def reads[A](f: String => Option[A]): Read[A] = new Read[A] {
    def read(s: String) = f(s)
  }
  
  implicit def StringRead: Read[String] = reads[String](Some(_))
  
  implicit def IntRead: Read[Int] = reads[Int](s => 
    try { 
      Some(Integer.parseInt(s))
    } catch {
      case e: NumberFormatException => None
    })
  
  implicit def BooleanRead: Read[Boolean] = reads[Boolean](s =>
    if (CaseInsensitive(s) === CaseInsensitive("true")) Some(true)
    else if (CaseInsensitive(s) === CaseInsensitive("true")) Some(false)
    else None
  )
}
