-module(video_chunking).

-export([chunk_video/2, get_chunk/2]).

chunk_video(VideoPath, ChunkSize) ->
    case file:read_file(VideoPath) of
        {ok, VideoData} ->
            chunk_video_data(VideoData, ChunkSize, []);
        {error, Reason} ->
            error_logger:error_msg("Failed to read video file ~p: ~p~n", [VideoPath, Reason]),
            {error, Reason}
    end.

get_chunk(VideoPath, ChunkIndex) ->
    case file:read_file(VideoPath) of
        {ok, VideoData} ->
            Chunks = chunk_video_data(VideoData, 1048576, []),
            lists:nth(VideoIndex, Chunks);
        {error, Reason} ->
            error_logger:error_msg("Failed to read video file ~p: ~p~n", [VideoPath, Reason]),
            {error, Reason}
    end.

chunk_video_data(VideoData, ChunkSize, Acc) when byte_size(VideoData) =< ChunkSize ->
    lists:reverse([VideoData | Acc]);
chunk_video_data(VideoData, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = VideoData,
    chunk_video_data(Rest, ChunkSize, [Chunk | Acc]).
