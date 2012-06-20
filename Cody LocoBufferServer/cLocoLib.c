//Cody Baxter
//Fall 2010, CS 493

#include <stdio.h>
#include <windows.h>
#include <string.h>
#include <conio.h>

static HANDLE hSerial;
static BOOL portOpen = 0;

int StartSerialPort(int comnum)
{
	int fail;
	char com[4];
	sprintf(com, "COM%i", comnum);
	hSerial = CreateFile(com, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	
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
	
	/*COMMTIMEOUTS timeouts = {0};
	timeouts.ReadIntervalTimeout = 1; //was 50
	timeouts.ReadTotalTimeoutConstant = 100; //was 50
	timeouts.ReadTotalTimeoutMultiplier = 1000; //was 10
	timeouts.WriteTotalTimeoutConstant = 0; //was 0
	timeouts.WriteTotalTimeoutMultiplier = 0; //was 0*/
	
	COMMTIMEOUTS timeouts = {0};
	timeouts.ReadIntervalTimeout = MAXDWORD; //was 50
	timeouts.ReadTotalTimeoutConstant = 0; //was 50
	timeouts.ReadTotalTimeoutMultiplier = 0; //was 10
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

//Used for internal testing only
////////////////////////////////
// int main (int argc, char *argv[])
// {
 // unsigned __int8 Data;
 // unsigned __int8 Data2;
 // unsigned __int8 read;
 // int ComResult;
	// ComResult = StartSerialPort(3);
	// printf("%d\n", ComResult);
	// // if (ComResult == 0)
	// // {
	// // Data = (unsigned __int8) 131; //0x83
	// // Data2 = (unsigned __int8) 124; //0x7c
	// // WriteToSerial(Data);
	// // WriteToSerial(Data2);
	// // WriteToSerial((unsigned __int8) 176);
	// // WriteToSerial((unsigned __int8) 9);
	// // WriteToSerial((unsigned __int8) 16);
	// // WriteToSerial((unsigned __int8) 86);
	// // while (1)
	// // {
		// // read = ReadFromSerial();
		// // printf("%x\n", read);
	// // }
	// // }
	
	// return 0;
// }
////////////////////////////////