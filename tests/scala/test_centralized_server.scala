import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._

class TestCentralizedServer extends AnyFlatSpec with Matchers {

  "CentralizedServer" should "start the centralized server" in {
    val result = Await.result(CentralizedServer.startServer(8080), 5.seconds)
    result shouldBe ()
  }

  it should "stop the centralized server" in {
    val result = Await.result(CentralizedServer.stopServer(), 5.seconds)
    result shouldBe ()
  }

  it should "handle client requests" in {
    val clientSocket = new java.net.Socket("localhost", 8080)
    val result = Await.result(CentralizedServer.handleClientRequest(clientSocket), 5.seconds)
    result shouldBe ()
  }
}
