start startlocobufferserver.exe TRACE yes  
start StartController.exe IP 127.0.0.1 PORT 1236 TRACE yes
start RBLDisplay.exe
start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE Layout.xml KEYBOARDLOG no ADMINLOG yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE LabReadSwitches.xml KEYBOARDLOG no ADMINLOG yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE standalone KEYBOARDLOG no ADMINLOG yes
rem start Throttle.exe IP 127.0.0.1 PORT 1236
rem start Throttle.exe IP 127.0.0.1 PORT 1236
