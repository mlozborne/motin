--LocoBuffer Package
--4/5/2011
WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Api39dll; USE Api39dll;
WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
WITH Ada.Exceptions; USE Ada.Exceptions;

PACKAGE BODY LocoBuffer IS

   CZero : C.Double := C.Double(0);

   --------------------------------------------------------------
   -- sets up server
   -- accepts clients, adds clients to socket list
   --------------------------------------------------------------
   TASK BODY ListenForLocoBufferClientsTaskType IS
      CValue, ListenSocket, AcceptSocket : C.Double;
   BEGIN
      --set up server
      CValue := DllInit;
      LOOP                                                                    -- mo 12/17/11
         ListenSocket := TCPListen(
            Port=>LocoBufferPort, 
            Max=>C.Double(2),
            Mode=>C.Double(0.0));        -- blocking
         EXIT WHEN Integer(ListenSocket) > 0;
      END LOOP;
      LOOP--loop to add clients
         BEGIN
            LOOP--loop until client accepted                                          -- mo 12/17/11
               AcceptSocket := TcpAccept(Sockid=>ListenSocket, Mode=> C.Double(0));     -- blocking
               EXIT WHEN Integer(AcceptSocket) > 0;
            END LOOP;
            FOR I IN SocketListArray'RANGE LOOP
               --loop to add to socket list
               IF (Integer(SocketListArray(I)) < 0) THEN
                  SocketListArray(I) := AcceptSocket;
                  EXIT;
               END IF;
            END LOOP;
            put_line("LocoBuffer pkg in ListenForClientTask: accepted client connection");
???            DELAY 0.01; --0.1;                                                    -- mo 12/17/11
         EXCEPTION
            WHEN error: OTHERS =>
               put_line("**************** EXCEPTION in LocoBuffer pkg: ListenForLocoBufferClientsTaskType " & Exception_Information(Error));
         END;
      END LOOP;
   END ListenForLocoBufferClientsTaskType;

   ---------------------------------------------------------------
   --  read arrays of bytes from each client in socket list
   --  filter out messages that are not LocoNet messages
   --  send to LocoBuffer one byte at a time
   ---------------------------------------------------------------

   FUNCTION makeChecksumByte (
         ByteArray : ByteArrayType;
         Size      : Integer) RETURN Unsigned_8 IS
      Checksum : Unsigned_8;
   BEGIN
???      Checksum := 16#ff# XOR ByteArray(1);
      FOR I IN 2..(Size-1) LOOP
         Checksum := Checksum XOR ByteArray(I);
      END LOOP;
      RETURN Checksum;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in LocoBuffer.makeChecksumByte function --" & kLFString & Exception_Information (error));
         RAISE;
   END makeChecksumByte;
   
   TASK BODY WriteLocoBufferStringTaskType IS
      Size         : Integer;
      MyArray      : ByteArrayType;
      Checksum     : Unsigned_8;
      CValue       : C.Double;
      messageCount : natural := 0;
      
   BEGIN
      LOOP
         BEGIN
            FOR I IN SocketListArray'RANGE LOOP
               --read from each client in socketlist
               IF (Integer(SocketListArray(I)) >= 0) THEN
                  --read from client
                  CValue := ClearBuffer(CZero);
                  Size := Integer(ReceiveMessage(SocketListArray(I), CZero, CZero));
                  FOR J IN 1..Size LOOP
                     MyArray(J) := Unsigned_8(ReadByte(CZero));
                  END LOOP;
                  --check if message is valid
???                  IF (MyArray(1) AND 16#80#) = 16#80# THEN
                     --is first byte opcode? leading bit should be 1
                     messageCount := messageCount + 1;
                     put_line("<" & natural'image(messageCount) & " writing valid message received from client");
                     MyArray(Size) := makeChecksumByte(MyArray, Size);
                     FOR K IN 1..Size LOOP
                        WriteData(MyArray(K));
                     END LOOP;
                  END IF;
               END IF;
            END LOOP;
???            DELAY 0.01; --0.1;                                                -- mo 12/17/11
         EXCEPTION
            WHEN Error: OTHERS=>
               put_line("**************** EXCEPTION in LocoBuffer pkg: WriteLocoBufferStringTaskType " & Exception_Information(Error));
         END;
      END LOOP;
   END WriteLocoBufferStringTaskType;

   --------------------------------------------------------------
   --  set up LocoBuffer connection
   --  read bytes from LocoBuffer until message is assembled
   --  send array of bytes to clients
   --------------------------------------------------------------
   TASK BODY ReadLocoBufferByteTaskType IS
      Data            : Unsigned_8;
      CValue          : C.Double;
      MyArray         : ByteArrayType;
      InUse           : Natural;
      Size, ReturnVal : Integer;
      messageCount    : natural := 0;
   BEGIN
      --Initialize locoBuffer port
      put_line("LocoBuffer pkg in ReadLocoBufferByteTask: trying to connect to LocoBuffer");
      LOOP
         ReturnVal := InitializePort;
         put_line("LocoBufferPkg in  ReadLocoBufferByteTask: initialize port returns" & Integer'Image(ReturnVal));
         EXIT WHEN ReturnVal > 0;
      END LOOP;
      put_line("LocoBuffer pkg in  ReadLocoBufferByteTask: connected to LocoBuffer");
      
      LOOP
         BEGIN
            --read single message from locobuffer
            Data := ReadData;
            IF (Data AND 16#80#) = 16#80# THEN
               --if first bit is 1, is opcode
               --how many bytes follow?
               IF (Data AND 16#F0#) = 16#80# THEN--only Checksum follows
                  InUse := 2;
               ELSIF (Data AND 16#F0#) = 16#A0# THEN --2 bytes plus Checksum
                  InUse := 4;
               ELSIF (Data AND 16#F0#) = 16#B0# THEN --2 bytes plus Checksum
                  InUse := 4;
               ELSIF (Data AND 16#F0#) = 16#E0# THEN --12 bytes plus Checksum
                  InUse := 14;
               ELSE
                  InUse := 1;
               END IF;

               MyArray(1) := Data;
               FOR I IN 2..InUse LOOP
                  Data := ReadData;
                  MyArray(I) := Data;
               END LOOP;
               CValue := ClearBuffer(CZero);--clear buffer
               FOR J IN 1..InUse LOOP--write array to buffer
                  Cvalue := WriteByte(C.Double(MyArray(J)), CZero);
               END LOOP;
               messageCount := messageCount + 1;
               put_line(">" & natural'image(messageCount) & " read message from railroad trying to send to clients");
               FOR I IN SocketListArray'RANGE LOOP--write to all clients
                  IF (Integer(SocketListArray(I)) >= 0) THEN
                     put_line("   sent to client at socket" & integer'image((Integer(SocketListArray(I)))));
                     Size := Integer(SendMessage(SocketListArray(I), New_String(""), CZero, CZero));
                  END IF;
               END LOOP;
            END IF;
???            -- DELAY 0.01; --0.1;                                       -- mo 12/17/11
         EXCEPTION
            WHEN error: OTHERS =>
               put_line("**************** EXCEPTION in LocoBuffer pkg  in  ReadLocoBufferByteTask: ReadLocoByte " & Exception_Information(Error));
         END;
      END LOOP;
   END ReadLocoBufferByteTaskType;
END LocoBuffer;

