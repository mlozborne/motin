start startlocobufferserver.exe TRACE yes  
rem start RailroadBig.exe
start Throttle.exe IP 127.0.0.1 PORT 1236
start Throttle.exe IP 127.0.0.1 PORT 1236
start StartController.exe IP 127.0.0.1 PORT 1236 TRACE yes
start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE LabSetSwitches.xml KEYBOARDLOG no ADMINLOG yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE LabReadSwitches.xml KEYBOARDLOG no ADMINLOG yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE standalone KEYBOARDLOG no ADMINLOG yes
