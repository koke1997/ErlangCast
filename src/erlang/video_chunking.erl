-module(video_chunking).

-export([chunk_video/2, get_chunk/2]).

% Chunks a video file into smaller chunks of the specified size
chunk_video(VideoPath, ChunkSize) ->
    % Read the video file
    case file:read_file(VideoPath) of
        {ok, VideoData} ->
            % Chunk the video data
            chunk_video_data(VideoData, ChunkSize, []);
        {error, Reason} ->
            % Log an error message if reading the file fails
            error_logger:error_msg("Failed to read video file ~p: ~p~n", [VideoPath, Reason]),
            {error, Reason}
    end.

% Retrieves a specific chunk of a video file
get_chunk(VideoPath, ChunkIndex) ->
    % Read the video file
    case file:read_file(VideoPath) of
        {ok, VideoData} ->
            % Chunk the video data
            Chunks = chunk_video_data(VideoData, 1048576, []),
            % Retrieve the specified chunk
            lists:nth(VideoIndex, Chunks);
        {error, Reason} ->
            % Log an error message if reading the file fails
            error_logger:error_msg("Failed to read video file ~p: ~p~n", [VideoPath, Reason]),
            {error, Reason}
    end.

% Helper function to chunk the video data
chunk_video_data(VideoData, ChunkSize, Acc) when byte_size(VideoData) =< ChunkSize ->
    % Return the accumulated chunks in reverse order
    lists:reverse([VideoData | Acc]);
chunk_video_data(VideoData, ChunkSize, Acc) ->
    % Split the video data into a chunk and the remaining data
    <<Chunk:ChunkSize/binary, Rest/binary>> = VideoData,
    % Recursively chunk the remaining data
    chunk_video_data(Rest, ChunkSize, [Chunk | Acc]).
