#include <stdio.h>
#include <windows.h>
#include <string.h>
#include <conio.h>

#int initializeCOMPort();
int StartSerialPort();

#unsigned_int8 readFromCOM();
unsigned_int8 ReadFromSerial();

#void writeToCOM(unsigned_int8 write);
void WriteToSerial(unsigned_int8 write);