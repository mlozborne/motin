start RailroadBig
rem start StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes
rem start RBLDisplay.exe IP 127.0.0.1 PORT 1235
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE Layout.xml KEYBOARDLOG no ADMINLOG yes
rem start AdminThrottle.exe IP 127.0.0.1 PORT 1234 MODE standalone KEYBOARDLOG no ADMINLOG yes
rem start Throttle.exe IP 127.0.0.1 PORT 1234
pause
m