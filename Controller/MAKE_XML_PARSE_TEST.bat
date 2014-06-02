rem Build XMLParseTest
gnatmake XMLParseTest -aI../Util/XMLAda/sax  -aI../Util -aI../Util/XMLAda/input_sources -aI../Util/XMLAda/unicode -D obj -aO../Util -largs ../util/CLocoBuffer.o -l39dll
move XMLParseTest.exe ../RunSoftware
pause
