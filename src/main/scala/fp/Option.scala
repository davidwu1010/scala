package fp

import java.util.Random

object Option extends App {
  val config: Map[String, String] = Map(
    "host" -> "176.45.36.1",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected"
  }

  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  val host = config.get("host")
  val port = config.get("port")
  val connection = host.flatMap(h => port.flatMap(p => Connection(h, p)))
  val connectionStatus = connection.map(c => c.connect)
  connectionStatus.foreach(println)

  config.get("host")
    .flatMap(h =>
      config.get("port").flatMap(p => Connection(h, p)))
    .map(c => c.connect)
    .foreach(println)

  val newConnectionStatus = for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
  } yield connection
  newConnectionStatus.foreach(println)
}
