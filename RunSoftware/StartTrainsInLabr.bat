start startlocobufferserver.exe TRACE yes  
rem start RailroadBig.exe
rem start Throttle.exe 
rem start StartController.exe IP 127.0.0.1 PORT 1236 TRACE yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE lab.xml KEYBOARDLOG yes ADMINLOG yes
start AdminThrottle.exe IP 127.0.0.1 PORT 1236 MODE standalone KEYBOARDLOG no ADMINLOG yes
