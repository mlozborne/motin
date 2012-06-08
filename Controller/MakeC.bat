rem Build StartController
gnatmake StartController -aILayoutManager -aI../Util -aI../Util/XMLAda/sax -aI../Util/XMLAda/input_sources -aI../Util/XMLAda/unicode -D obj -aO../Util -largs ../util/CLocoBuffer.o -l39dll 
move StartController.exe ../RunSoftware
pause
