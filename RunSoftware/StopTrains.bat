rem these lines work with Windows XP and Windows 8
tskill startlocobufferserver
tskill adminthrottle
tskill throttle
tskill StartController
tskill railroadbig
tskill RBLDisplay

rem these lines work with Windows 7 and Windows 8
taskkill /T /IM startlocobufferserver.exe
taskkill /T /IM adminthrottle.exe
taskkill /T /IM throttle.exe
taskkill /T /IM StartController.exe
taskkill /T /IM railroadbig.exe
taskkill /T /IM RBLDisplay.exe
