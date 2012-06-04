--LocoBuffer Package
--4/5/2011

WITH Interfaces; USE Interfaces;
WITH Interfaces.C; USE Interfaces.C;

PACKAGE LocoBuffer IS

   FUNCTION InitializePort RETURN Integer;
   FUNCTION ReadData RETURN Interfaces.Unsigned_8;
   PROCEDURE WriteData(Data: Interfaces.Unsigned_8);

   --sets up server
   --listens for clients
   --adds to client list
   TASK TYPE ListenForLocoBufferClientsTaskType IS
   END ListenForLocoBufferClientsTaskType;

   --recieves messages from the clients
   --filters out messages which are not LocoNet messages
   --send to LocoBuffer
   TASK TYPE WriteLocoBufferStringTaskType IS

   END WriteLocoBufferStringTaskType;

   --initializes LocoBuffer port
   --reads bytes from LocoBuffer until message assembled
   --sends message to client
   TASK TYPE ReadLocoBufferByteTaskType IS

   END ReadLocoBufferByteTaskType;

   TYPE SocketListType IS ARRAY (0..19) OF C.Double;
   SocketListArray: SocketListType := (OTHERS=>(-1.0));
PRIVATE
   PRAGMA Import(C, InitializePort, "StartSerialPort");
   PRAGMA Import(C, ReadData, "ReadFromSerial");
   PRAGMA Import(C, WriteData, "WriteToSerial");


END LocoBuffer;


