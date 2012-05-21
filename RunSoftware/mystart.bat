start ..\GMSimulators\railroadbig.exe
PING -n 2 127.0.0.1>nul

start ..\GMSimulators\throttle.exe 
PING -n 2 127.0.0.1>nul

rem port 1234: simulator listens for throttles and/or contoller
rem          : used for desktop debugging of software

rem port 1235: controller listens for throttles
           
rem port 1236: loco buffer server listens for throlles and/or controller
rem          : used in lab to connect software to the loconet

rem options for TaskStarter.exe
rem trace/noTrace: for display of internal behavior of controller
start ..\controller\taskstarter.exe ip 127.0.0.1 port 1234 notrace
PING -n 2 127.0.0.1>nul

rem options for AdminThrottle.exe
rem controller/standalone mode
rem keyboardLog/noKeyboardLog
rem adminLog/noAdminLog
start ..\adminthrottle\adminthrottle.exe ip 127.0.0.1 port 1235 controller nokeyboardLog noadminLog
PING -n 2 127.0.0.1>nul
