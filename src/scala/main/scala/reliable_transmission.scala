import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Success, Failure}

object ReliableTransmission {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val retryLimit = 5
  private val ackTimeout = 1000

  def sendData(peer: String, data: String): Future[Unit] = {
    def attemptSend(retryCount: Int): Future[Unit] = {
      if (retryCount >= retryLimit) {
        Future.failed(new Exception("Retry limit reached"))
      } else {
        // Simulate sending data to peer
        println(s"Sending data to $peer: $data")
        // Simulate waiting for acknowledgment
        Future {
          Thread.sleep(ackTimeout)
          if (scala.util.Random.nextBoolean()) {
            println(s"Acknowledgment received from $peer")
            Success(())
          } else {
            println(s"No acknowledgment from $peer, retrying...")
            Failure(new Exception("No acknowledgment"))
          }
        }.flatMap {
          case Success(_) => Future.successful(())
          case Failure(_) => attemptSend(retryCount + 1)
        }
      }
    }

    attemptSend(0)
  }

  def receiveData(dataHandler: String => Unit): Future[Unit] = Future {
    // Simulate receiving data
    val data = "Received data"
    println(data)
    dataHandler(data)
    // Simulate sending acknowledgment
    println("Sending acknowledgment")
  }

  def handleTransmissionError(peer: String, reason: String): Unit = {
    println(s"Transmission error with peer $peer: $reason")
  }
}
