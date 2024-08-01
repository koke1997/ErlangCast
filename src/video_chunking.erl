-module(video_chunking).

-export([chunk_video/2, get_chunk/2]).

chunk_video(VideoPath, ChunkSize) ->
    {ok, VideoData} = file:read_file(VideoPath),
    chunk_video_data(VideoData, ChunkSize, []).

get_chunk(VideoPath, ChunkIndex) ->
    {ok, VideoData} = file:read_file(VideoPath),
    Chunks = chunk_video_data(VideoData, 1048576, []),
    lists:nth(VideoIndex, Chunks).

chunk_video_data(VideoData, ChunkSize, Acc) when byte_size(VideoData) =< ChunkSize ->
    lists:reverse([VideoData | Acc]);
chunk_video_data(VideoData, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = VideoData,
    chunk_video_data(Rest, ChunkSize, [Chunk | Acc]).
