rem Build XMLParseTest
gnatmake XMLParseTest -aI../Util/XMLAda/sax  -aI../Util -aI../Util/XMLAda/input_sources -aI../Util/XMLAda/unicode -D obj 
move XMLParseTest.exe ../RunSoftware
pause
