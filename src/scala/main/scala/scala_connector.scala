import org.apache.pekko.actor.{Actor, ActorSystem, Props}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object ScalaConnector {
  implicit val timeout: Timeout = Timeout(5.seconds)
  val system: ActorSystem = ActorSystem("ScalaErlangSystem")
  val erlangActor = system.actorOf(Props[ErlangActor], "erlangActor")

  def sendToErlang(message: String): Unit = {
    if (system.whenTerminated.isCompleted) {
      restart()
    } else {
      erlangActor ! message
    }
  }

  def receiveFromErlang(): Future[String] = {
    (erlangActor ? "receive").mapTo[String]
  }

  def restart(): Future[Unit] = {
    for {
      _ <- system.terminate()
      _ <- system.whenTerminated
      newSystem = ActorSystem("ScalaErlangSystem")
      newErlangActor = newSystem.actorOf(Props[ErlangActor], "erlangActor")
    } yield {
      system = newSystem
      erlangActor = newErlangActor
    }
  }

  class ErlangActor extends Actor {
    def receive: Receive = {
      case message: String =>
        // Handle message from Scala to Erlang
        println(s"Sending message to Erlang: $message")
        // Simulate sending message to Erlang
        sender() ! "ack"
      case "receive" =>
        // Simulate receiving message from Erlang
        sender() ! "Message from Erlang"
    }
  }
}
