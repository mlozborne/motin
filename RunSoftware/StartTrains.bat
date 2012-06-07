echo off
cls

start railroadbig.exe
PING -n 2 127.0.0.1>nul

start throttle.exe 
PING -n 2 127.0.0.1>nul

rem start startlocobufferserver.exe 
rem PING -n 2 127.0.0.1>nul

start StartController.exe ip 127.0.0.1 port 1234 notrace
PING -n 2 127.0.0.1>nul

start adminthrottle.exe ip 127.0.0.1 port 1235 controller nokeyboardLog adminLog
PING -n 2 127.0.0.1>nul

