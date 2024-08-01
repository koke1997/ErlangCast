import unittest
from peer_discovery import start, stop, handle_message, broadcast_message

class TestPeerDiscovery(unittest.TestCase):

    def test_start(self):
        result = start()
        self.assertEqual(result, 'ok')

    def test_stop(self):
        result = stop()
        self.assertEqual(result, 'ok')

    def test_handle_message(self):
        result = handle_message("Test message")
        self.assertEqual(result, 'ok')

    def test_broadcast_message(self):
        result = broadcast_message("Test message")
        self.assertEqual(result, 'ok')

if __name__ == '__main__':
    unittest.main()
