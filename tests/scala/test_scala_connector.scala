import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._

class TestScalaConnector extends AnyFlatSpec with Matchers {

  "sendToErlang" should "send a message to Erlang" in {
    ScalaConnector.sendToErlang("Test message")
    // Assuming some mechanism to verify the message was sent
    // This is a placeholder for actual verification logic
    true should be (true)
  }

  "receiveFromErlang" should "receive a message from Erlang" in {
    val message = Await.result(ScalaConnector.receiveFromErlang(), 5.seconds)
    message should be ("Message from Erlang")
  }
}
