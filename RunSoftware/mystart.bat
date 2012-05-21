start ..\GMSimulators\railroadbig.exe
PING -n 2 127.0.0.1>nul

start ..\GMSimulators\throttle.exe 
PING -n 2 127.0.0.1>nul

rem port 1234 = simulator
rem port 1235 = controller
rem port 1236 = loco buffer server

start ..\controller\taskstarter.exe ip 127.0.0.1 railPort 1234 noTrace
PING -n 2 127.0.0.1>nul


start ..\adminthrottle\adminthrottle.exe ip 127.0.0.1 port 1235 controller keyboardLog adminLog
PING -n 2 127.0.0.1>nul
