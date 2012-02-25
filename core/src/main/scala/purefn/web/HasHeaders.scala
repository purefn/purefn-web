package purefn.web

trait HasHeaders[F] {
  def updateHeaders(f: Headers => Headers)(a: F): F
  def headers(a: F): Headers
}

object HasHeaders {
  @inline def apply[F](implicit F: HasHeaders[F]): HasHeaders[F] = F
}
