import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Success, Failure}

object FaultTolerance {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private var isRunning = false
  private var errorCount = 0

  def start(): Future[Unit] = Future {
    isRunning = true
    println("Fault tolerance service started.")
  }

  def stop(): Future[Unit] = Future {
    isRunning = false
    println("Fault tolerance service stopped.")
  }

  def handleError(error: String): Future[Unit] = Future {
    println(s"Handling error: $error")
    errorCount += 1
  }

  def recover(error: String): Future[Unit] = Future {
    println(s"Recovering from error: $error")
    errorCount -= 1
  }
}
