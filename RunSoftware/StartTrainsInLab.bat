start startlocobufferserver.exe TRACE yes  
start StartController.exe IP 127.0.0.1 PORT 1236 TRACE yes
start RBLDisplay.exe IP 127.0.0.1 PORT 1235
start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE Layout.xml KEYBOARDLOG no ADMINLOG yesstart Throttle.exe IP 127.0.0.1 PORT 1236
start Throttle.exe IP 127.0.0.1 PORT 1236
start Throttle.exe IP 127.0.0.1 PORT 1236
start Throttle.exe IP 127.0.0.1 PORT 1236
