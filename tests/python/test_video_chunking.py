import unittest
from video_chunking import chunk_video, get_chunk

class TestVideoChunking(unittest.TestCase):

    def test_chunk_video(self):
        video_path = "path/to/test_video.mp4"
        chunk_size = 1048576
        result = chunk_video(video_path, chunk_size)
        self.assertEqual(len(result), 5)  # Assuming the video is chunked into 5 parts

    def test_get_chunk(self):
        video_path = "path/to/test_video.mp4"
        chunk_index = 1
        result = get_chunk(video_path, chunk_index)
        self.assertEqual(len(result), 1048576)  # Assuming each chunk is 1MB

if __name__ == '__main__':
    unittest.main()
