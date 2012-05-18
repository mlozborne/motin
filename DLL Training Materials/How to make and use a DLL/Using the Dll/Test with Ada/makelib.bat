@echo off
dll2def %1.dll > %1.def
dlltool --dllname %1.dll --def %1.def --output-lib lib%1.a
if exist dh.o del dh.o
if exist dt.o del dt.o
if exist ds0.o del ds0.o
