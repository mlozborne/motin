rem Build StartLocoBufferServer
gnatmake StartLocoBufferServer -aI../Util -D obj -aO../Util -largs ../util/CLocoBuffer.o -l39dll
move  StartLocoBufferServer.exe ../RunSoftware