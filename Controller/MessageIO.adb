--MessageIO package body
--4/8/2011
WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
WITH Interfaces; USE Interfaces; 
WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Api39dll; USE Api39dll;
WITH CommandQueueManager; USE CommandQueueManager;
WITH ControllerGlobals; USE ControllerGlobals;
WITH Ada.Exceptions; USE  Ada.Exceptions;
with MessageTranslationLibrary; use MessageTranslationLibrary;  -- mo 1/7/12
with MessageTranslationTypes; use messageTranslationTypes;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tracer; use Tracer;


PACKAGE BODY MessageIO IS

	protected body LastMessageManagerType is
	
		procedure saveMessage(msg : messageType) is
		begin
			lastMessageSentToRailroad := msg;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION MesIOPkg saveMessage " & Exception_Information(Error));
            raise;
		end saveMessage;
		
		procedure isThisAnEcho(msg : messageType; echo : out boolean) is
			msg2  : messageType := lastMessageSentToRailroad;
		begin
			lastMessageSentToRailroad := kEmptyMessage;
			if msg.size /= msg2.size then
				echo := false;
				return;
			 end if;
			
			if msg.byteArray(1..msg.size) = msg2.byteArray(1..msg2.size) then
			   echo := true;
				return;
			else
				echo := false;
				return;
			end if;			
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION MesIOPkg isThisAnEcho " & Exception_Information(Error));
            raise;
		end isThisAnEcho;
		
   end LastMessageManagerType;

   CZero : C.Double := C.Double (0);

   -------------------------------------------------
   -- set up server
   -- continuously waits for throttle connections
   -- add new connections to socketlist
   -------------------------------------------------
   TASK BODY ListenForThrottleTaskType IS
      ListenSocket,
         AcceptSocket,
         CValue       : C.Double;
   BEGIN
      CValue := DllInit;
      
      LOOP                                               
         put_line("MessageIO pkg in ListenForThrottleTask: setting up server for OThrottles at 1235");
         ListenSocket := TcpListen(
            Port => MessageIOPort,
            Max  => C.Double (2),
            Mode => C.Double (0));             -- 0 here causes blocking on TcpAccept
         EXIT WHEN Integer(ListenSocket) > 0;
         put_line("MessageIO pkg in ListenForThrottleTask: error in setting up server for OThrottles");
      END LOOP;
      put_line("MessageIO pkg in ListenForThrottleTask: server for OThrottles now set up");
      
      LOOP--loop to add clients
         LOOP--loop to accept connections
            put_line("MessageIO pkg in ListenForThrottleTask: listening for OThrottle");
            AcceptSocket := TcpAccept(Sockid => ListenSocket, Mode => C.Double (1));    -- 1 here causes nonblocking on ReceiveMessage
            EXIT WHEN Integer(AcceptSocket) > 0;
            put_line("MessageIO pkg in ListenForThrottleTask: error in listening for OThrottle");
            --DELAY 1.0;                 -- test 6                        
         END LOOP;
         put_line("MessageIO pkg in ListenForThrottleTask: new OThrottle at socket " & C.Double'Image(AcceptSocket));
         SocketList.AddSocket(AcceptSocket);--add to socketlist
      END LOOP;
   EXCEPTION
      WHEN error: OTHERS =>
         put_line("**************** EXCEPTION in MessageIO: ListenForThrottleTaskType " & Exception_Information(Error));
   END ListenForThrottleTaskType;

   -----------------------------------------------------
   -- connects to LocoBuffer package server or simulator
   -- stores socket in socketlist, end task
   -----------------------------------------------------
   TASK BODY ConnectToSimulatorOrLocoBufferTaskType IS
      ConnectSocket, CValue        : C.Double;
   BEGIN
      CValue := DllInit;
      
      LOOP
         IF Simulator THEN
            put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: trying to connect to train simulator");
            ConnectSocket := TcpConnect(
               Ip   => New_String (to_string(IpStrAda)),
               Port => SimulatorPort,
               Mode => C.Double (1));            -- Does this cause nonblocking on SendMessage????
         ELSE
            put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: trying to connect to loco buffer");
            ConnectSocket := TcpConnect(
               Ip=>New_String(to_string(IpStrAda)), 
               Port=> LocoBufferPort, 
               Mode=>C.Double(1));               -- Does this cause nonblocking on SendMessage????
         END IF;
         EXIT WHEN Integer(ConnectSocket) > 0;
         put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: error when trying to connect");
      END LOOP;
      
      IF Simulator THEN
         put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: connected to train simulator");
      ELSE
         put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: connected to LocoBuffer pkg server");
      END IF;
      
      SocketList.AddRailroadSocket(ConnectSocket);
   EXCEPTION
      WHEN error: OTHERS =>
         put_line("**************** EXCEPTION in MessageIO: ConnectToSimulatorOrLocoBufferTaskType " & Exception_Information(Error));
   END ConnectToSimulatorOrLocoBufferTaskType;

   ---------------------------------------------------------
   -- get message from CommandQueueManager
   -- convert internal message into LocoNet Message
   -- send message to relevant sockets
   ---------------------------------------------------------
   TASK BODY SendMessageTaskType IS
      internalMessage : MessageType;
      MyArray         : ByteArrayType;
      CValue,
      Socket          : C.Double;
      Slot            : SlotType;
      Size,
      SocketListLen   : Integer;
      messageCount    : natural := 0;
   BEGIN
      loop
         SocketList.GetSocket(0, Socket);
         exit when integer(socket) > 0;
         delay 1.0;
      end loop;
      
      LOOP
         BEGIN
            CommandQueueManager.Get(InternalMessage);
            MyArray := internalMessage.ByteArray;
                        
            CValue := ClearBuffer(CZero);--clear buffer
            CASE MyArray(1) IS
			
               WHEN OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>  --send to railroad slot
			   
                  Slot := SlotLookupTable.TrainIdToPhysSlotNum(Integer(MyArray(2)));
                  MyArray(2) := Unsigned_8(Slot);
                  MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                  SocketList.GetSocket(0, Socket);
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
				      
						internalMessage.byteArray := myArray;
						LastMessageManager.saveMessage(internalMessage);
				  
			   WHEN OPC_LOCO_ADR | OPC_MOVE_SLOTS | OPC_WR_SL_DATA | OPC_GPON | OPC_GPOFF | OPC_SW_REQ => --send to railroad slot
			   
                  SocketList.GetSocket(0, Socket);
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
				  
						LastMessageManager.saveMessage(internalMessage);
						
						if myArray(1) = OPC_GPON then
							ignoreMessagesUntilTime := clock + ignoreDurationForPowerOn; 
						end if;
						if myArray(1) = OPC_GPOFF then
							ignoreMessagesUntilTime := clock + ignoreDurationForPowerOff; 
					   end if;
			  
            WHEN OPC_SL_RD_DATA =>             --send to all throttles, not railroad
			   
                  Slot := SlotLookupTable.TrainIdToVirtSlotNum(Integer(MyArray(3)));
                  MyArray(3) := Unsigned_8(Slot);
                  MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  SocketList.GetSocketListLength(SocketListLen);
                  FOR I IN 1..(SocketListLen-1) LOOP
                     SocketList.GetSocket(I, Socket);
                     IF Integer(Socket) > 0 THEN
                        Size := Integer(SendMessage(Socket, New_String(""),CZero, CZero));
                     END IF;
                  END LOOP;
				  
               WHEN OPC_LONG_ACK | OPC_SW_REP =>    --send to all throttles, not railroad
			   
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  SocketList.GetSocketListLength(SocketListLen);
                  FOR I IN 1..(SocketListLen-1) LOOP
                     SocketList.GetSocket(I, Socket);
                     IF Integer(Socket) > 0 THEN
                        Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
                     END IF;
                  END LOOP;
				  
               WHEN UZero =>--extended messages
			   
                  CASE MyArray(2) IS
                     WHEN PutTrainState | PutTrainPosition | PutTrainInformation =>
                        Slot := SlotLookupTable.TrainIdToVirtSlotNum( Integer(MyArray(3)));
                        MyArray(3) := Unsigned_8(Slot);					
                     WHEN OTHERS =>
                        NULL;
                  END CASE;
                  --send all extended messages to OThrottles
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  SocketList.GetSocketListLength(SocketListLen);
                  FOR I IN 1..(SocketListLen-1) LOOP
                     SocketList.GetSocket(I, Socket);
                     IF Integer(Socket) > 0 THEN
                        Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
                     END IF;
                  END LOOP;
               WHEN OTHERS=>
                  null;
            END CASE;

				internalMessage.byteArray := myArray;
            if myArray(1) /= uzero then
                messageCount := messageCount + 1;
                myPutLine("<" & natural'image(messageCount) & " " & toEnglish(internalMessage) & " ...MesIOPkg.SendMesTask to outside");      
				else
                myPutLine("< " & toEnglish(internalMessage) & " ...MesIOPkg.SendMesTask to outside");      
            end if;			
            myPutLine(" ");
			
            --DELAY 0.01; --0.1;            test 5  necessary here                                  -- mo 12/17/11
			
         EXCEPTION
            WHEN Error : OTHERS =>
               put_line("**************** EXCEPTION in MessageIO: SendMessageTaskType ( " & Exception_Information(Error) & " )");
         END;
      END LOOP;
   END SendMessageTaskType;
		
   ----------------------------------------------------------
   -- loop to Receive messages from sockets
   -- convert LocoNet message to internal message
   -- pass message to CommandQueueManager
   ----------------------------------------------------------
   TASK BODY ReceiveMessageTaskType IS
      Length          : Integer;
      Socket,
      CValue          : C.Double;
      Size            : Integer;
      MyArray         : ByteArrayType; --array of unsigned_8
      internalMessage : MessageType;
      TrainId         : TrainIdType;
      messageCount    : natural := 0;
      found           : boolean;
		messagesEqual   : boolean;
		ignoringMessages: boolean;
		
   BEGIN
      loop
         SocketList.GetSocket(0, Socket);
         exit when integer(socket) > 0;
         delay 1.0;
      end loop;

      LOOP
         BEGIN
            SocketList.GetSocketListLength(Length);
            FOR I IN 0..(Length-1) LOOP
               --read from each socket in socketlist
               SocketList.GetSocket(I, Socket);
               IF (Integer(Socket) = -1) THEN
                  null;
               else -- read from client
                  CValue := ClearBuffer(CZero);
                  Size := Integer(ReceiveMessage(Socket, CZero, CZero));
                  IF Size <= 0 THEN
                     null;
                  else
						
                     FOR J IN 1..Size LOOP
                        MyArray(J) := Unsigned_8(ReadByte(CZero));
                     END LOOP;

							-- Echo the message
							internalMessage.Size := natural(Size);
							internalMessage.ByteArray := MyArray;
							myPutLine(" ");
							if i /= 0 then
								-- not from simulator/locobuffer server
								myPutLine("> " & toEnglish(internalMessage) & " ... MesIOPkg.RecMesTask");
							else
								-- from simulator/locobuffer server
								messageCount := messageCount + 1;
								myPutLine(">" & natural'image(messageCount) & " " & toEnglish(internalMessage) & " ... MesIOPkg.RecMesTask");
							end if;
							
							ignoringMessages := (clock <= ignoreMessagesUntilTime);							
							if ignoringMessages then
								myPutLine("       ... ignoring messages during power on/off processing ");
						   end if;
					 
							-- if this message is from the simulator/locobuffer server and 
							-- identical to the last message sent to the simulator/locobuffer server then ignore it
							messagesEqual := false;
							if not ignoringMessages and i = 0 then
								lastMessageManager.isThisAnEcho(internalMessage, messagesEqual);
								if messagesEqual then
									myPutLine("       ... message from railroad ignored because equals last message sent to railroad ");
								end if;
							end if;
							if not ignoringMessages and (i /= 0 or (i = 0 and not messagesEqual)) then
								CASE MyArray(1) IS
									WHEN OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
										SlotLookupTable.VirtSlotNumToTrainId(Integer(MyArray(2)), trainId, found);
										if found then
											internalMessage.ByteArray(2) := Unsigned_8(TrainId);
											CommandQueueManager.put(InternalMessage);
										else  
											myPutLine("       ... message ignored because not using an established virtual slot number ");
										end if;
									WHEN OPC_LOCO_ADR | OPC_MOVE_SLOTS | OPC_SW_REQ =>                          -- discard if from railroad/simulator
										IF I = 0 THEN
											myPutLine("       ... ignored because from simulator/locobuffer");
									   else
										  CommandQueueManager.put(InternalMessage);
										END IF;
									WHEN OPC_LONG_ACK =>                        -- Discard if not from railroad/simulator
										IF I = 0 THEN
										   CommandQueueManager.put(InternalMessage);
									   else
											myPutLine("       ... ignored because not from simulator/locobuffer");
										END IF;
									WHEN OPC_SL_RD_DATA =>
										CommandQueueManager.put(InternalMessage);
									WHEN OTHERS =>
										CommandQueueManager.put(InternalMessage);
								END CASE;
							end if;
						end if;
               END IF;
            END LOOP;
            DELAY 0.01; --0.1;                         -- mo 12/17/11   test 4 necessary here
         EXCEPTION
            WHEN Error : OTHERS =>
               put_line("**************** EXCEPTION in MessageIO: ReceiveMessageTask ( " & Exception_Information(Error) & " )");
         END;
      END LOOP;
   END ReceiveMessageTaskType;

END MessageIO;

