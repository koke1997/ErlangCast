import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Await
import scala.concurrent.duration._

class TestPeerDiscovery extends AnyFlatSpec with Matchers {

  "PeerDiscovery" should "start the peer discovery process" in {
    val result = Await.result(PeerDiscovery.start(), 5.seconds)
    result shouldBe ()
  }

  it should "stop the peer discovery process" in {
    val result = Await.result(PeerDiscovery.stop(), 5.seconds)
    result shouldBe ()
  }

  it should "handle incoming peer discovery messages" in {
    val result = Await.result(PeerDiscovery.handleMessage("Test message"), 5.seconds)
    result shouldBe ()
  }

  it should "broadcast peer discovery messages" in {
    val result = Await.result(PeerDiscovery.broadcastMessage("Test message"), 5.seconds)
    result shouldBe ()
  }

  it should "restart the peer discovery service" in {
    val result = Await.result(PeerDiscovery.restart(), 5.seconds)
    result shouldBe ()
  }
}
