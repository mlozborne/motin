echo off
cls

rem --------------PORT USAGE----------------------

rem 1234     SIMULATOR listens for throttles and/or contoller
rem          used for desktop debugging of software

rem 1235     CONTROLLER listens for throttles     
     
rem 1236     LOCOBUFFERSERVER listens for throttles and/or controller
rem          used in lab to connect software to the loconet

rem --------------GAMEMAKER SIMULATOR----------------------

rem    no parameters needed

rem --------------GAMEMAKER THROTTLE----------------------

rem    no parameters needed
rem    once started must select the ip address and the connection port in a popup dialog: 
rem      simulator or locobuffer server with physical loco address to bypass the controller if there is one
rem      simulator or locobuffer server with virtual loco address to involve the controller

rem --------------CONTROLLER--------------------

rem    ip address of computer hosting the connection port
rem    port can be one of: simulator, locobuffer server
rem    trace/notrace: for display of internal behavior of controller
rem    If running all throttles in standalone mode, DO NOT RUN the controller


rem ---------------ADMINTHROTTLE---------------

rem    ip address of computer hosting the connection port
rem    port can be one of: 
rem         simulator, locobuffer server in standalone mode
rem         controller in controller mode (throttle must be given name of layout file)
rem    controller/standalone mode
rem    keyboardLog/noKeyboardLog
rem    adminLog/noAdminLog

rem ---------------LOCOBUFFERSERVER---------------

rem    no parameters needed

rem ---------------START THE DESIRED PROGRAMS HERE--------------------

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

