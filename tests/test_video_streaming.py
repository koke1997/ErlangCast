import unittest
import os
from video_streaming import start_hls_streaming, start_dash_streaming, get_hls_playlist, get_dash_manifest

class TestVideoStreaming(unittest.TestCase):

    def setUp(self):
        self.video_path = "path/to/test_video.mp4"
        self.output_dir = "path/to/output"
        os.makedirs(self.output_dir, exist_ok=True)

    def tearDown(self):
        for file in os.listdir(self.output_dir):
            file_path = os.path.join(self.output_dir, file)
            if os.path.isfile(file_path):
                os.unlink(file_path)
        os.rmdir(self.output_dir)

    def test_start_hls_streaming(self):
        result = start_hls_streaming(self.video_path, self.output_dir)
        self.assertEqual(result, 'ok')

    def test_get_hls_playlist(self):
        start_hls_streaming(self.video_path, self.output_dir)
        result = get_hls_playlist(self.output_dir)
        self.assertTrue(result.endswith(".m3u8"))

    def test_start_dash_streaming(self):
        result = start_dash_streaming(self.video_path, self.output_dir)
        self.assertEqual(result, 'ok')

    def test_get_dash_manifest(self):
        start_dash_streaming(self.video_path, self.output_dir)
        result = get_dash_manifest(self.output_dir)
        self.assertTrue(result.endswith(".mpd"))

if __name__ == '__main__':
    unittest.main()
