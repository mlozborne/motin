--LocoBuffer Package
--4/5/2011
WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Api39dll; USE Api39dll;
WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
WITH Ada.Exceptions; USE Ada.Exceptions;
with messageTranslationTypes; use messageTranslationTypes;
with messageTranslationLibrary; use messageTranslationLibrary;
with Tracer; use Tracer;

PACKAGE BODY LocoBuffer IS

   CZero : C.Double := C.Double(0);

   --------------------------------------------------------------
   -- sets up server
   -- accepts clients, adds clients to socket list
   --------------------------------------------------------------
   TASK BODY ListenForLocoBufferClientsTaskType IS
         ListenSocket		: socketType;    -- this socket will block
			AcceptSocket      : socketType;    -- this socket will NOT block
   BEGIN
      initialize39DLL;      
                                                     
		put_line("LocoBuffer pkg in ListenForLocoBufferClientsTaskType: setting up server for LocoBuffer at 1236");
		establishListenerSocket(listenSocket, locoBufferPort, 51, true);
		if integer(listenSocket) <= 0 then
			put_line("Couldn't establish listener socket. Error code is " & toString(listenSocket));
			raise socketNotEstablished;
		end if;
      
      LOOP--loop to add clients
			acceptClient(listenSocket, acceptSocket, 1.0, false);
         put_line("LocoBuffer pkg in ListenForLocoBufferClientsTaskType: new OThrottle at socket " & toString(AcceptSocket));
			FOR I IN SocketListArray'RANGE LOOP    -- loop to add to socket list
				IF (Integer(SocketListArray(I)) < 0) THEN
					SocketListArray(I) := AcceptSocket;
					EXIT;
				END IF;
			END LOOP;
      END LOOP;
   EXCEPTION
      WHEN error: OTHERS =>
         put_line("**************** EXCEPTION in LocoBuffer: ListenForLocoBufferClientsTaskType " & Exception_Information(Error));
   END ListenForLocoBufferClientsTaskType;

   -- TASK BODY ListenForLocoBufferClientsTaskType IS
      -- CValue, ListenSocket, AcceptSocket : C.Double;
   -- BEGIN
      -- --set up server
      -- CValue := DllInit;
      -- LOOP                                                                    -- mo 12/17/11
         -- ListenSocket := TCPListen(
            -- Port=>LocoBufferPort, 
            -- Max=>C.Double(51),
            -- Mode=>C.Double(0.0));        -- blocking 
         -- EXIT WHEN Integer(ListenSocket) > 0;
      -- END LOOP;
      -- LOOP--loop to add clients
         -- BEGIN
            -- LOOP--loop until client accepted                                          -- mo 12/17/11
               -- AcceptSocket := TcpAccept(Sockid=>ListenSocket, Mode=> C.Double(1));     -- non blocking ???
               -- EXIT WHEN Integer(AcceptSocket) > 0;
					-- delay 1.0;   -- No point in checking more frequently
            -- END LOOP;
            -- FOR I IN SocketListArray'RANGE LOOP
            -- -- loop to add to socket list
               -- IF (Integer(SocketListArray(I)) < 0) THEN
                  -- SocketListArray(I) := AcceptSocket;
                  -- EXIT;
               -- END IF;
            -- END LOOP;
            -- put_line("LocoBuffer pkg in ListenForClientTask: accepted client connection");
         -- EXCEPTION
            -- WHEN error: OTHERS =>
               -- put_line("**************** EXCEPTION in LocoBuffer pkg: ListenForLocoBufferClientsTaskType " & Exception_Information(Error));
         -- END;
      -- END LOOP;
   -- END ListenForLocoBufferClientsTaskType;

   FUNCTION makeChecksumByte (
         ByteArray : ByteArrayType;
         Size      : Integer) RETURN Unsigned_8 IS
      Checksum : Unsigned_8;
   BEGIN
      Checksum := 16#ff# XOR ByteArray(1); ---???
      FOR I IN 2..(Size-1) LOOP
         Checksum := Checksum XOR ByteArray(I);
      END LOOP;
      RETURN Checksum;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in LocoBuffer.makeChecksumByte function --" & kLFString & Exception_Information (error));
         RAISE;
   END makeChecksumByte;
   
   ---------------------------------------------------------------
   --  read arrays of bytes from each client in socket list
   --  filter out messages that are not LocoNet messages
   --  send to LocoBuffer one byte at a time
   ---------------------------------------------------------------

   TASK BODY WriteLocoBufferStringTaskType IS
      Size         : Integer;
      -- CValue       : C.Double;
      messageCount : natural := 0;
      msg          : messageType;
      
   BEGIN
      LOOP
         BEGIN
            FOR I IN SocketListArray'RANGE LOOP
               --read from each client in socketlist
               IF Integer(SocketListArray(I)) >= 0 THEN
                  --read from client
						receiveMessage(socketListArray(i), msg, size);
                  -- CValue := ClearBuffer(CZero);
                  -- Size := Integer(ReceiveMessage(SocketListArray(I), CZero, CZero));
						if size <= 0 then
							null;
						else
                      -- FOR J IN 1..Size LOOP
                         -- MyArray(J) := Unsigned_8(ReadByte(CZero));
                      -- END LOOP;
                      --check if message is valid
                      IF (msg.byteArray(1) AND 16#80#) = 16#80# THEN  
                         --is first byte opcode? leading bit should be 1
                         msg.byteArray(Size) := makeChecksumByte(msg.byteArray, Size);
								 -- myPutLine("Top of WriteData loop");
                         FOR K IN 1..Size LOOP
                            WriteData(msg.byteArray(K));
									 -- myPutLine("Finish one WriteData");
                         END LOOP;
                         
                         -- msg.byteArray := myArray;
                         -- msg.size := size;                         
                         messageCount := messageCount + 1;
                         myPutLine("<" & natural'image(messageCount) 
								            & " " & toEnglish(msg)
                                    & " // write to railroad; received on socket" 
                                    & integer'image(Integer(SocketListArray(I))));                                      
                      END IF;
                  end if;
               END IF;
            END LOOP;
				DELAY 0.001; -- locobuffer has limited write baud rate, mustn't overwhelm it
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
      -- CValue          : C.Double;
      -- MyArray         : ByteArrayType;
      InUse           : Natural;
      Size, ReturnVal : Integer;
      messageCount    : natural := 0;
      msg             : messageType;
   BEGIN
      --Initialize locoBuffer port
      put_line("LocoBuffer pkg in ReadLocoBufferByteTask: trying to connect to LocoBuffer");
      LOOP
         ReturnVal := InitializePort;
         put_line("LocoBuffer pkg in  ReadLocoBufferByteTask: initialize port returns" & Integer'Image(ReturnVal));
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
               IF (Data AND 16#F0#) = 16#80# THEN
                  InUse := 2;
               ELSIF (Data AND 16#F0#) = 16#A0# THEN 
                  InUse := 4;
               ELSIF (Data AND 16#F0#) = 16#B0# THEN 
                  InUse := 4;
               ELSIF Data = 16#EF# OR Data = 16#E7# THEN 
                  InUse := 14;
               ELSIF Data = 16#E5#  THEN 
                  InUse := 16;
               ELSIF Data = 16#ED#  THEN 
                  InUse := 11;
               ELSE
                  InUse := 0;
               END IF;
               
					if inUse /= 0 then
						msg.byteArray(1) := Data;
						FOR I IN 2..InUse LOOP
						  Data := ReadData;
						  -- delay 0.0001;
						  msg.byteArray(I) := Data;
						END LOOP;
						-- CValue := ClearBuffer(CZero);--clear buffer
						-- FOR J IN 1..InUse LOOP--write array to buffer
						  -- Cvalue := WriteByte(C.Double(msg.byteArray(J)), CZero);
						-- END LOOP;
						
						messageCount := messageCount + 1;
						-- msg.byteArray := myArray;
						msg.size := inUse;
						myPutLine(">" & natural'image(messageCount) & " " & toEnglish(msg) & " // read from railroad");
						
						FOR I IN SocketListArray'RANGE LOOP--write to all clients
						  IF (Integer(SocketListArray(I)) >= 0) THEN
							 sendMessage(socketListArray(i), msg, size);
							 -- Size := Integer(SendMessage(SocketListArray(I), New_String(""), CZero, CZero));
							 -- myPutLine("      sent to socket" 
							           -- & integer'image((Integer(SocketListArray(I))))
										  -- & " with size =" & integer'image(size));
							 --delay 0.001; -- Not all messages are reaching the controller. Perhaps this will fix it.
						  END IF;
						END LOOP;
					end if;
				   
            END IF;
            DELAY 0.1; -- locobuffer has limited read baud rate and loconet not going to generate 
                        -- more than x message/sec				
         EXCEPTION
            WHEN error: OTHERS =>
               put_line("**************** EXCEPTION in LocoBuffer pkg  in  ReadLocoBufferByteTask " & Exception_Information(Error));
         END;
      END LOOP;
   END ReadLocoBufferByteTaskType;
END LocoBuffer;

