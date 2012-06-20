--Cody Baxter
--Fall 2010, CS 493

WITH LocoBufferLib;
WITH Interfaces;
USE Interfaces;

PACKAGE BODY LocoBufferLib IS

   --Records for clients----------------------------------------
   MAX_CLIENTS : constant := 20;
   totalClients : Integer := 0;
   type ClientRecord is record
      socket : C.Double := 0.0;
      active : Boolean := False;
   end record;
   ClientList : array (1..MAX_CLIENTS) of ClientRecord;
   -------------------------------------------------------------

   --Bit array declerations and mics.---------------------------
   TYPE BitArray IS ARRAY (1 .. 8) OF Integer;
   Byte : BitArray;
   Display : Integer := 3;
   -------------------------------------------------------------

   -------------------------------------------------------------
   PROCEDURE IntegerToBitArrayR (BArray : IN OUT BitArray; IntToConvert : IN Integer);
   -------------------------------------------------------------

   --Declerations for TCP/IP Connection-------------------------
   ListenSocket,
   AcceptSocket       : C.Double;
   CValue, CCValue : C.Double;
   BufferIdRecv : C.Double := C.Double (0);
   BufferIdSend : C.Double := C.Double (1);
   UserPort           : Integer;
   CEmptyString : Chars_Ptr := New_String ("");
   -------------------------------------------------------------

   --Task Declerations------------------------------------------
   Task_WriteCom : WriteCom;
   Task_WriteSA : WriteSA;
   Task_Read  : Read;
   Task_TCPIP : TCPIPConnection;
   Alive : Integer := 1;
   -------------------------------------------------------------

   --StartLocoLib, starts all connections and task needed for adaLocoLib-------
   ----------------------------------------------------------------------------
   PROCEDURE StartLocoLib IS
      BoolInt : Integer := 0;
      UserInput : Integer := 3;
      ComStatus : Integer := 0;
   BEGIN
      Put_Line("Welcome to adaLocoLib server"); New_Line;

      --Attempt com connection
      Put_Line ("Trying Locobuffer connection on Com port 3.");
      Put_Line ("If connection fails, you will get a chance to change the values.");
      Loop
         ComStatus := StartComConnection(UserInput);
         Exit When ComStatus = 1 OR UserInput = 0 OR UserInput = -1;

         begin
            New_Line;
            Put_Line("Com connection failed"); New_Line;
            Put("Enter new com port number, 0 for standalone mode, or -1 to quit: ");
            Get (UserInput);
            if UserInput = -1 then
               RETURN;
            end if;
         end;
      end loop;

      --Ask user if they would like information displayed, when recieved from
      --TCP/IP connection and Com port
      New_Line;
      Put_Line ("Data that is read from com and recieved from TCP/IP connections");
      Put_Line ("is automatically displayed. You can change that by typing the following");
      Put_Line ("once everything is running:");
      Put_Line ("No data: 0");
      Put_Line ("Data from com port only: 1");
      Put_Line ("Data from TCP/IP connections only: 2");
      Put_Line ("All data: 3");
      New_Line;


      --Get port number from user
      Put_Line("Starting up server."); New_Line;
      Put ("Enter port number or 0 for default port of 14804: ");
      Get (Userport);
      if Userport = 0 then
         Userport := 14804;
      end if;

      --start tcp/ip task
      New_Line;
      Put_Line ("Starting TCPIP Connections");
	  CValue := DLLInit;
	  CCValue := DLLInit;
      ListenSocket := TCPListen(
                                Port   => C.Double(UserPort),
                                Max => C.Double (2),
                                Mode => C.Double (1.0));
put ("listen socket = "); put (integer (ListenSocket));
      IF Integer(ListenSocket) <= 0 THEN
         Put ("Failed to listen on port: ");
         Put (UserPort);
         New_Line;
         Return;
      ELSE
         Put ("Listening on port: ");
         Put (UserPort);
         New_Line;
      END IF;
      Task_TCPIP.StartAccepting;

      if UserInput /= 0 then
         --start read task
         Task_Read.StartRead;
         --start write task
         Task_WriteCom.StartWriteCom;
      else
         Task_WriteSA.StartWriteSA;
      end if;

      while TRUE loop
         Get (display);
         if (display = 0) Then
            Put_Line ("Set display to no data");
         end if;
         if (display = 1) Then
            Put_Line ("Set display to com data only");
         end if;
         if (display = 2) Then
            Put_Line ("Set display to TCP/IP data only");
         end if;
         if (display = 3) Then
            Put_Line ("Set display to all data");
         end if;
      end loop;

   END StartLocoLib;
   ----------------------------------------------------------------------------

   --StartTCPIPConnection, starts connection to AdaRail program----------------
   ----------------------------------------------------------------------------
   Task Body TCPIPConnection IS
      --d : Duration;
   BEGIN
      Accept StartAccepting;
      --d := To_Duration(10);
      Put_Line("Server running with IP address of local machine.");
      while Alive = 1 loop
         AcceptSocket := TcpAccept (
                                    Sockid => ListenSocket,
                                    Mode => C.Double (1.0));

         If (Integer(AcceptSocket) > 0) AND (totalClients < MAX_CLIENTS) THEN
            Put("Accepted connection"); New_Line;
            --find first nonactive record
            for i in 1 .. MAX_CLIENTS loop
               if ClientList(i).active = False then
                  ClientList(i).socket := AcceptSocket;
                  ClientList(i).active := True;
                  totalClients := totalClients + 1;
                  exit;
               end if;
            end loop;
         END IF;

         delay 2.0;
      End Loop;
   END TCPIPConnection;
   ----------------------------------------------------------------------------

   --StartComConnection, starts to the connection to the LocoBuffer------------
   ----------------------------------------------------------------------------
   FUNCTION StartComConnection (ComNum : Integer) RETURN Integer IS
   BEGIN
      Return StartPort(ComNum);
   END StartComConnection;
   ----------------------------------------------------------------------------

   --Body of write task, reads from TCP/IP and sends to com connection---------
   ----------------------------------------------------------------------------
   TASK BODY WriteCom IS
      --Use CValue for Recieveing
      msg : MessageType;
	  displays : Print;
   BEGIN
      ACCEPT StartWriteCom;
      Put_Line("Write to com task running");
	  displays := getprint;
      while Alive = 1 LOOP
         --Put_Line ("Write task started. Waiting for message on TCP/IP");
         for i in 1..MAX_CLIENTS loop
            if ClientList(i).active = true then
               ----------------------------------------------------------
			   CValue := ClearBuffer (BufferIdRecv);
			   Cvalue := ReceiveMessage(              -- blocking
                               Sockid => clientlist(i).socket,
                               Len    => C.Double(0),
                               Buffid => BufferIdRecv);
			   msg.InUse := integer(cvalue);
			   for i in 1..msg.InUse loop
                   msg.bytes(i) := unsigned_8(readByte(BufferIdRecv));
               end loop;
			   ----------------------------------------------------------

               if msg.InUse = 0 Then
                  --Temp := CloseSocket(ClientList(i).socket);
                  Put ("Client "); Put(i, width => 1); Put_Line(" has disconnected.");
                  ClientList(i).socket := 0.0;
                  ClientList(i).active := False;
                  totalClients := totalClients - 1;
               end if;

               if msg.InUse > 0 THEN
                  FOR i IN 1..msg.InUse LOOP
                     WriteToCom(msg.bytes(i));
                  END LOOP;
                  --if (Display = 2 OR Display = 3) then
                  if (Display = 2) OR (Display = 3) then
                     displays.printMessage (msg, TCPIP);
                  end if;
               End If;

            end if;
         End Loop;
      END LOOP;

   END WriteCom;
   ----------------------------------------------------------------------------

   --Body of writeSA task, reads from TCP/IP and back to TCP/IP---------
   ----------------------------------------------------------------------------
   TASK BODY WriteSA IS
      --Use CValue for Recieveing
      error  : Integer;
      msg : MessageType;
	  displays : Print;
   BEGIN
      Accept StartWriteSA;
      Put_Line("Write in standalone task running");
	  displays := getprint;
      while Alive = 1 Loop
         for i in 1..MAX_CLIENTS loop
            if ClientList(i).active = true then
               ----------------------------------------------------------
			   CValue := ClearBuffer (BufferIdRecv);
			   Cvalue := ReceiveMessage(              -- blocking
                               Sockid => clientlist(i).socket,
                               Len    => C.Double(0),
                               Buffid => BufferIdRecv);
			   msg.InUse := integer(cvalue);
			   for i in 1..msg.InUse loop
                   msg.bytes(i) := unsigned_8(readByte(BufferIdRecv));
               end loop;
			   ----------------------------------------------------------

               if msg.InUse = 0 Then
                  --Temp := CloseSocket(ClientList(i).socket);
                  Put ("Client "); Put(i, width => 1); Put_Line(" has disconnected.");
                  ClientList(i).socket := 0.0;
                  ClientList(i).active := False;
                  totalClients := totalClients - 1;
               end if;

               if msg.InUse > 0 THEN
                  --display message
                  if display /= 0 then
                     displays.printMessage (msg, TCPIP);
                  end if;

                  for i in 1..MAX_CLIENTS loop
                     if ClientList(i).active = true then
                        error := sendTCPMessage (msg, clientlist(i).socket);
                     end if;
                  end loop;
               end if;
            End If;

         End Loop;
      end loop;

   END WriteSA;
   ----------------------------------------------------------------------------

   --Body of read task, reads from com connection and sends to AdaRail over----
   --TCP/IP connection---------------------------------------------------------
   TASK BODY Read IS
      --Use CCValue for Sending
      BitRead : Unsigned_8;
      NumRead, Start : Integer;
      msg : MessageType;
	  displays : Print;
   BEGIN
      ACCEPT StartRead;
      Put_Line("Read from com task running");
      --all opcodes start with 1 in the most signifact bit
      --check for length of message. i.e. 2 4 6 mulit
      --checksum is repeated until new opcode comes across
	  displays := getprint;
      While Alive = 1 LOOP

         BitRead := ReadFromCom;

         IntegerToBitArrayR(Byte, Integer(BitRead));
         IF Byte(8) = 1 THEN --Opcode
            msg.bytes(1) := BitRead;

            if ((byte(7) = 0) and (byte(6) = 0)) then --2 byte message
               msg.InUse := 2;
               NumRead := 2;
               Start := 2;
            end if;
            if ((byte(7) = 0) and (byte(6) = 1)) then --4 byte message
               msg.InUse := 4;
               NumRead := 4;
               Start := 2;
            end if;
            if ((byte(7) = 1) and (byte(6) = 0)) then --6 byte message
               msg.InUse := 6;
               numRead := 6;
               Start := 2;
            end if;
            if ((byte(7) = 1) and (byte(6) = 1)) then --n byte message
               BitRead := ReadFromCom;
               msg.bytes(2) := BitRead;
               msg.InUse := Integer(BitRead);
               numRead := msg.InUse - 1;
               Start := 3;
            end if;


            FOR i IN Start..NumRead - 1 LOOP
               BitRead := ReadFromCom;
               msg.bytes(i) := BitRead;
            END LOOP;

            --display message
            if (display = 1) or (Display = 3) then
               displays.printMessage (msg, COM);
            end if;

			CCValue := ClearBuffer (BufferIdSend);
			for i in 1..msg.InUse loop
			   CCValue := writebyte(c.double(msg.bytes(i)), BufferIdSend);
			end loop;

            for i in 1..MAX_CLIENTS loop
               if ClientList(i).active = true then
                  CCValue := sendmessage(clientlist(i).socket, CEmptyString, C.Double (0.0), BufferIdSend);
               end if;
            end loop;
         END IF;
      END LOOP;
   END Read;
   ----------------------------------------------------------------------------

   --IntegerToBitArray, takes integer and converts to bit array----------------
   ----------------------------------------------------------------------------
   PROCEDURE IntegerToBitArrayR (
                                 BArray       : IN OUT BitArray;
                                 IntToConvert : IN     Integer) IS
      TempInt     : Integer := IntToConvert;
      LoopCounter : Integer := 2;
   BEGIN
      BArray(1) := 0;
      BArray(2) := 0;
      BArray(3) := 0;
      BArray(4) := 0;
      BArray(5) := 0;
      BArray(6) := 0;
      BArray(7) := 0;
      BArray(8) := 0;
      BArray(1) := TempInt rem 2;
      TempInt := TempInt / 2;
      WHILE TempInt /= 0 LOOP
         BArray(LoopCounter) := TempInt rem 2;
         TempInt := TempInt / 2;
         LoopCounter := LoopCounter + 1;
      END LOOP;
   END IntegerToBitArrayR;
   ----------------------------------------------------------------------------

END LocoBufferLib;
