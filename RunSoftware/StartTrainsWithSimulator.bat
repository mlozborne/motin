rem start startlocobufferserver.exe  
start RailroadBig.exe
rem start Throttle.exe 
rem start StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE simulator.xml KEYBOARDLOG no ADMINLOG yes
start AdminThrottle.exe IP 127.0.0.1 PORT 1234 MODE standalone KEYBOARDLOG no ADMINLOG yes
