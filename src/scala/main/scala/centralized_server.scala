import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}
import java.net.{ServerSocket, Socket}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}

object CentralizedServer {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private var serverSocket: Option[ServerSocket] = None
  private var isRunning = false

  def startServer(port: Int): Future[Unit] = Future {
    if (isRunning) {
      restartServer(port)
    } else {
      serverSocket = Some(new ServerSocket(port))
      isRunning = true
      println(s"Centralized server started on port $port")

      while (isRunning) {
        val clientSocket = serverSocket.get.accept()
        handleClientRequest(clientSocket)
      }
    }
  }

  def stopServer(): Future[Unit] = Future {
    isRunning = false
    serverSocket.foreach(_.close())
    println("Centralized server stopped")
  }

  def handleClientRequest(clientSocket: Socket): Future[Unit] = Future {
    val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
    val out = new PrintWriter(clientSocket.getOutputStream, true)

    val request = in.readLine()
    println(s"Received request: $request")

    // Handle the request and send a response
    val response = s"Response to: $request"
    out.println(response)

    clientSocket.close()
  }

  def restartServer(port: Int): Future[Unit] = {
    for {
      _ <- stopServer()
      _ <- startServer(port)
    } yield ()
  }
}
