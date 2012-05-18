@echo off
if "%1" == "clean" (echo y | rm -f *.adb.* *.ads.*) else (gnatmake XMLParseTest.adb -gnat05 -aIXMLAda/sax -aIXMLAda/input_sources -aIXMLAda/unicode -aI.. -D ..\obj)