-module(video_chunking_tests).

-include_lib("eunit/include/eunit.hrl").

chunk_video_test() ->
    VideoPath = "path/to/test_video.mp4",
    ChunkSize = 1048576,
    {ok, VideoData} = file:read_file(VideoPath),
    {ok, Chunks} = video_chunking:chunk_video(VideoPath, ChunkSize),
    ?assertEqual(1048576, byte_size(lists:nth(1, Chunks))).

chunk_video_edge_case_test() ->
    VideoPath = "path/to/non_existent_video.mp4",
    ChunkSize = 1048576,
    ?assertException(error, _, video_chunking:chunk_video(VideoPath, ChunkSize)).

chunk_video_invalid_input_test() ->
    VideoPath = "path/to/test_video.mp4",
    InvalidChunkSize = -1,
    ?assertException(error, _, video_chunking:chunk_video(VideoPath, InvalidChunkSize)).

get_chunk_test() ->
    VideoPath = "path/to/test_video.mp4",
    ChunkIndex = 1,
    {ok, VideoData} = file:read_file(VideoPath),
    {ok, Chunk} = video_chunking:get_chunk(VideoPath, ChunkIndex),
    ?assertEqual(1048576, byte_size(Chunk)).

get_chunk_edge_case_test() ->
    VideoPath = "path/to/non_existent_video.mp4",
    ChunkIndex = 1,
    ?assertException(error, _, video_chunking:get_chunk(VideoPath, ChunkIndex)).

get_chunk_invalid_input_test() ->
    VideoPath = "path/to/test_video.mp4",
    InvalidChunkIndex = -1,
    ?assertException(error, _, video_chunking:get_chunk(VideoPath, InvalidChunkIndex)).
