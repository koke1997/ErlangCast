-module(video_chunking_tests).

-include_lib("eunit/include/eunit.hrl").

chunk_video_test() ->
    VideoPath = "path/to/test_video.mp4",
    ChunkSize = 1048576,
    {ok, Chunks} = video_chunking:chunk_video(VideoPath, ChunkSize),
    ?assertEqual(1048576, byte_size(lists:nth(1, Chunks))),
    ?assertEqual(1048576, byte_size(lists:nth(2, Chunks))),
    ?assertEqual(1048576, byte_size(lists:nth(3, Chunks))).

get_chunk_test() ->
    VideoPath = "path/to/test_video.mp4",
    ChunkIndex = 1,
    {ok, Chunk} = video_chunking:get_chunk(VideoPath, ChunkIndex),
    ?assertEqual(1048576, byte_size(Chunk)).
