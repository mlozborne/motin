gcc -c clocolib.c
gnatmake -c StartLocoBufferServer.adb -O0 -l39dll
gnatbind StartLocoBufferServer.ali
gnatlink StartLocoBufferServer.ali clocolib.c -l39dll
DEL *.o
DEL *.ali