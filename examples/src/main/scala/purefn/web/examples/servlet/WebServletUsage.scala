package purefn.web.examples
package servlet

import purefn.web._, servlet._, PathTemplate._, Route._, Web.{writeStr, addResponseHeader}
import purefn.web.{CaseInsensitive => CI}

import scalaz.std.string._
import scalaz.syntax.monad._
import scalaz.effect.IO

import javax.servlet.ServletConfig

object WebServletUsage {

  def hello: Web[Unit] = 
    addResponseHeader[Web](CI("Content-Type"), "text/html") >>
    writeStr[Web]("<html><body><h3>Hello, world!</h3></body></html>")
  
  class HelloServlet extends WebServlet {
    def webInit(config: ServletConfig) = route[Web, Unit](** -> hello).point[IO]
  }
  
  def main(args: Array[String]): Unit = {
    import org.eclipse.jetty.server.Server, org.eclipse.jetty.servlet._
    
    val server = new Server(8080)
    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setContextPath("/")
    server.setHandler(context)
    context.addServlet(new ServletHolder(new HelloServlet()),"/*");
 
    server.start();
    server.join();
  }
}