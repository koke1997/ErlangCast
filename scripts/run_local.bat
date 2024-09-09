@echo off

REM Run the project locally on Windows

REM Call the script to install dependencies
call scripts\install_dependencies.bat

REM Use rebar3 to compile the project
rebar3 compile

REM Use rebar3 to run the project
rebar3 shell

REM Print success message after running
echo The project is now running locally.
