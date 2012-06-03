rem This bat file can run
rem     GameMaker simulator
rem     GameMaker throttle
rem     controller program
rem     admin throttle program

rem ... For each exe prefix the path from the folder containing mystart.bat
rem ... to the exe.

rem --------------GameMaker simulator----------------------
start ..\GMSimulators\railroadbig.exe
PING -n 2 127.0.0.1>nul

rem --------------GameMaker throttle----------------------
start ..\GMSimulators\throttle.exe 
PING -n 2 127.0.0.1>nul

rem --------------ports----------------------
rem port 1234: simulator listens for throttles and/or contoller
rem          : used for desktop debugging of software

rem port 1235: controller listens for throttles
           
rem port 1236: loco buffer server listens for throlles and/or controller
rem          : used in lab to connect software to the loconet

rem --------------controller--------------------
rem options for TaskStarter.exe
rem trace/noTrace: for display of internal behavior of controller
rem When running all throttles in standalone mode, DO NOT RUN taskstarter.exe
rem start ..\controller\taskstarter.exe ip 127.0.0.1 port 1234 notrace
rem PING -n 2 127.0.0.1>nul

rem ---------------adminthrottle---------------
rem options for AdminThrottle.exe
rem controller/standalone mode
rem keyboardLog/noKeyboardLog
rem adminLog/noAdminLog
start ..\adminthrottle\adminthrottle.exe ip 127.0.0.1 port 1234 standalone nokeyboardLog adminLog
PING -n 2 127.0.0.1>nul
