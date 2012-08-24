rem start startlocobufferserver.exe  
start RailroadBig.exe
start Throttle.exe IP 127.0.0.1 PORT 1234
start StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes
start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE labSetSwitches.xml KEYBOARDLOG no ADMINLOG yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1234 MODE standalone KEYBOARDLOG no ADMINLOG yes
start RBLDisplay.exe
