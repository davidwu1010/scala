package fp

import scala.util.{Random, Try}

object ATry extends App {
  val hostname = "localhost"
  val port = "8080"

  def renderHTML(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection =
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Someone else took the port")
  }

  Try(HttpService.getConnection(hostname, port))
    .flatMap(c => Try(c.get("/home")))
    .foreach(renderHTML)

  for {
    connection <- Try(HttpService.getConnection(hostname, port))
    page <- Try(connection.get("/home"))
  } renderHTML(page)
}
