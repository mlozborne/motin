start startlocobufferserver.exe  
rem start RailroadBig.exe
start Throttle.exe 
start StartController.exe IP 127.0.0.1 PORT 1236 TRACE yes
start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE lab.xml KEYBOARDLOG yes ADMINLOG yes
