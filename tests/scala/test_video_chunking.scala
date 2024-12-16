import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Failure}

class TestVideoChunking extends AnyFlatSpec with Matchers {

  "VideoChunking" should "chunk a video file into smaller chunks" in {
    val videoPath = "path/to/test_video.mp4"
    val chunkSize = 1048576
    VideoChunking.chunkVideo(videoPath, chunkSize) match {
      case Success(chunks) => chunks.length should be > 0
      case Failure(exception) => fail(s"Failed to chunk video: ${exception.getMessage}")
    }
  }

  it should "retrieve a specific chunk of a video file" in {
    val videoPath = "path/to/test_video.mp4"
    val chunkIndex = 1
    VideoChunking.getChunk(videoPath, chunkIndex) match {
      case Success(chunk) => chunk.length should be > 0
      case Failure(exception) => fail(s"Failed to retrieve chunk: ${exception.getMessage}")
    }
  }
}
