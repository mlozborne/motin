//Cody Baxter
//Spring 2011, CS 493

#include <stdio.h>
#include <windows.h>
#include <string.h>
#include <conio.h>

static HANDLE hSerial;
static BOOL portOpen = 0;

int StartSerialPort()
{
	int fail = 0;
	hSerial = CreateFile("COM3", GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	
	if (hSerial == INVALID_HANDLE_VALUE) //connection failed
		return 0;	
	
	DCB dcbSerialParams = {0};
	dcbSerialParams.DCBlength = sizeof(dcbSerialParams);
	dcbSerialParams.BaudRate = CBR_57600;
	dcbSerialParams.ByteSize = 8;
	dcbSerialParams.StopBits = ONESTOPBIT;
	dcbSerialParams.Parity = NOPARITY;
	dcbSerialParams.fOutxCtsFlow = 0;
	dcbSerialParams.fOutxDsrFlow = 0;
	dcbSerialParams.fOutX = 0;
	dcbSerialParams.fInX = 0;
	dcbSerialParams.fRtsControl = RTS_CONTROL_ENABLE;
	dcbSerialParams.fDtrControl = DTR_CONTROL_ENABLE;
	
	fail = SetCommState (hSerial, &dcbSerialParams);
	if (fail == 0) //SetCommState failed
		return 0;
	
	COMMTIMEOUTS timeouts = {0};
	timeouts.ReadIntervalTimeout = 1; //was 50
	timeouts.ReadTotalTimeoutConstant = 100; //was 50
	timeouts.ReadTotalTimeoutMultiplier = 1000; //was 10
	timeouts.WriteTotalTimeoutConstant = 0; //was 0
	timeouts.WriteTotalTimeoutMultiplier = 0; //was 0

	fail = SetCommTimeouts(hSerial, &timeouts);
	if (fail == 0) //SetCommTimeouts failed
		return 0;

	//Connection made
	portOpen = 1;
	return 1;
}

unsigned __int8 ReadFromSerial()
{
	DWORD bytesRead;
	unsigned __int8 read;
	BOOL readError;
	readError = ReadFile(hSerial, &read, 1, &bytesRead, NULL);
	return read;
}

void WriteToSerial(unsigned __int8 write)
{
	DWORD bytesWrote;
	BOOL writeError;
	writeError = WriteFile(hSerial, &write, 1, &bytesWrote, NULL);
	return;
}

