package purefn.web

trait HasHeaders[F] {
  def updateHeaders(a: F)(f: Headers => Headers): F
  def headers(a: F): Headers
}

object HasHeaders {
  @inline def apply[F](implicit F: HasHeaders[F]): HasHeaders[F] = F
}
