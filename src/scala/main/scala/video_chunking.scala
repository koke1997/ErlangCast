import java.io.{File, FileInputStream, IOException}
import java.nio.file.{Files, Paths}
import scala.util.{Try, Success, Failure}

object VideoChunking {

  def chunkVideo(videoPath: String, chunkSize: Int): Try[List[Array[Byte]]] = {
    Try {
      val videoFile = new File(videoPath)
      val videoData = Files.readAllBytes(videoFile.toPath)
      chunkVideoData(videoData, chunkSize, List.empty)
    }
  }

  def getChunk(videoPath: String, chunkIndex: Int): Try[Array[Byte]] = {
    chunkVideo(videoPath, 1048576).map { chunks =>
      if (chunkIndex >= 0 && chunkIndex < chunks.length) {
        chunks(chunkIndex)
      } else {
        throw new IndexOutOfBoundsException(s"Chunk index $chunkIndex out of bounds")
      }
    }
  }

  private def chunkVideoData(videoData: Array[Byte], chunkSize: Int, acc: List[Array[Byte]]): List[Array[Byte]] = {
    if (videoData.length <= chunkSize) {
      (videoData :: acc).reverse
    } else {
      val (chunk, rest) = videoData.splitAt(chunkSize)
      chunkVideoData(rest, chunkSize, chunk :: acc)
    }
  }
}
