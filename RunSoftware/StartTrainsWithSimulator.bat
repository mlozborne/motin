rem start startlocobufferserver.exe  
start RailroadBig.exe
start Throttle.exe 
start StartController.exe IP 127.0.0.1 PORT 1234 TRACE no
start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE simulator.xml KEYBOARDLOG no ADMINLOG no