# ErlangCast
ErlangCast is a P2P video streaming server built with Erlang, leveraging its strengths in concurrency and distributed programming. Features include peer discovery, video chunking, reliable data transmission, and fault-tolerant architecture. Ideal for real-time streaming and decentralized applications.

## Peer Discovery
The peer discovery feature allows peers to find each other in the network, enabling efficient communication and data sharing.

### How to Use Peer Discovery
1. Start the peer discovery process by calling the `peer_discovery:start/0` function.
2. Handle incoming peer discovery messages using the `peer_discovery:handle_message/1` function.
3. Broadcast peer discovery messages using the `peer_discovery:broadcast_message/1` function.
4. Stop the peer discovery process by calling the `peer_discovery:stop/0` function.

### Example
```erlang
% Start the peer discovery process
peer_discovery:start().

% Handle an incoming peer discovery message
peer_discovery:handle_message("Hello, peer!").

% Broadcast a peer discovery message
peer_discovery:broadcast_message("Hello, peers!").

% Stop the peer discovery process
peer_discovery:stop().
```
