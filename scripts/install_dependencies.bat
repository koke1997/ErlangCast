@echo off

REM Update Chocolatey
choco upgrade chocolatey

REM Install Erlang and rebar3
choco install erlang
choco install rebar3

REM Install ffmpeg
choco install ffmpeg

REM Print success message
echo All dependencies have been successfully installed.
