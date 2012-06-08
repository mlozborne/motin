echo off
cls
rem start startlocobufferserver.exe 
start RailroadBig.exe
start Throttle.exe 
start StartController.exe ip 127.0.0.1 port 1234 trace
start AdminThrottle.exe ip 127.0.0.1 port 1235 controller layoutfile simulator.xml nokeyboardLog adminLog
