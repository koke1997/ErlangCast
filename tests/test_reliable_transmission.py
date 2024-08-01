import unittest
from reliable_transmission import send_data, receive_data

class TestReliableTransmission(unittest.TestCase):

    def test_send_data(self):
        peer = "peer1"
        data = "Test data"
        result = send_data(peer, data)
        self.assertEqual(result, 'ok')

    def test_receive_data(self):
        data_handler = lambda data: self.assertEqual(data, "Test data")
        result = receive_data(data_handler)
        self.assertEqual(result, 'ok')

if __name__ == '__main__':
    unittest.main()
