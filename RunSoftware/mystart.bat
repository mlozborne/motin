start ..\GMSimulators\railroadbig.exe
PING -n 2 127.0.0.1>nul

start ..\GMSimulators\throttle.exe 
PING -n 2 127.0.0.1>nul

start ..\controller\taskstarter.exe
PING -n 2 127.0.0.1>nul

start ..\adminthrottle\adminthrottle.exe ip 127.0.0.1 port 1235  
PING -n 2 127.0.0.1>nul
