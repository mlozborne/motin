--MessageIO package body
--4/8/2011
WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
WITH Interfaces; USE Interfaces; 
WITH Ada.Text_IO; USE Ada.Text_IO;
--WITH Api39dll; USE Api39dll;
with tcpip; use tcpip;
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
      
      function inPowerChangeMode return boolean is
      begin
         return powerChangeMode;
      end inPowerChangeMode;
      
      procedure enterPowerChangeMode is 
      begin
         powerChangeMode := true;
      end enterPowerChangeMode;
      
      procedure messageReceived is 
      begin
         timeStampLastMessageReceived := clock;
      end messageReceived;
      
      procedure powerChangeCompletingNow(answer : out boolean) is
      begin
         if powerChangeMode and then clock - timeStampLastMessageReceived > powerChangeTimeOut then
            powerChangeMode := false;
            answer := true;
         else   
            answer := false;
         end if;
      end powerChangeCompletingNow;
      
   end LastMessageManagerType;

   -------------------------------------------------
   -- set up server
   -- continuously waits for throttle connections
   -- add new connections to socketlist
   -------------------------------------------------
   TASK BODY ListenForThrottleTaskType IS
         ListenSocket      : socketType;    -- this socket will block
         AcceptSocket      : socketType;    -- this socket will NOT block
   BEGIN
      initialize39DLL;      
                                                     
      put_line("MessageIO -- In ListenForThrottleTask: setting up server for OThrottles at 1235");
      establishListenerSocket(listenSocket, messageIOPort, 101, true);
      if integer(listenSocket) <= 0 then
         put_line("MessageIO -- Couldn't establish listener socket. Error code is " & toString(listenSocket));
         raise socketNotEstablished;
      end if;
      
      LOOP--loop to add clients
         acceptClient(listenSocket, acceptSocket, 1.0, false);
         put_line("MessageIO -- in ListenForThrottleTask: new OThrottle at socket " & toString(AcceptSocket));
         SocketList.AddSocket(AcceptSocket);--add to socketlist
      END LOOP;
   EXCEPTION
      WHEN error: OTHERS =>
         put_line("**************** EXCEPTION in MessageIO: ListenForThrottleTaskType " & Exception_Information(Error));
   END ListenForThrottleTaskType;

   -- TASK BODY ListenForThrottleTaskType IS
      -- ListenSocket,
         -- AcceptSocket,
         -- CValue       : C.Double;
   -- BEGIN
      -- CValue := DllInit;
      
      -- LOOP                                               
         -- put_line("MessageIO pkg in ListenForThrottleTask: setting up server for OThrottles at 1235");
         -- ListenSocket := TcpListen(
            -- Port => MessageIOPort,
            -- Max  => C.Double (2),
            -- Mode => C.Double (0));             -- 0 here causes blocking on TcpAccept
         -- EXIT WHEN Integer(ListenSocket) > 0;
         -- put_line("MessageIO pkg in ListenForThrottleTask: error in setting up server for OThrottles");
      -- END LOOP;
      -- put_line("MessageIO pkg in ListenForThrottleTask: server for OThrottles now set up");
      
      -- LOOP--loop to add clients
         -- LOOP--loop to accept connections
            -- put_line("MessageIO pkg in ListenForThrottleTask: listening for OThrottle");
            -- AcceptSocket := TcpAccept(
               -- Sockid => ListenSocket, 
               -- Mode => C.Double (1));    -- 1 here causes nonblocking on ReceiveMessage
            -- EXIT WHEN Integer(AcceptSocket) > 0;
            -- put_line("MessageIO pkg in ListenForThrottleTask: error in listening for OThrottle");
            -- DELAY 1.0;   -- only need to listen occassionally for throttle connections                     
         -- END LOOP;
         -- put_line("MessageIO pkg in ListenForThrottleTask: new OThrottle at socket " & C.Double'Image(AcceptSocket));
         -- SocketList.AddSocket(AcceptSocket);--add to socketlist
      -- END LOOP;
   -- EXCEPTION
      -- WHEN error: OTHERS =>
         -- put_line("**************** EXCEPTION in MessageIO: ListenForThrottleTaskType " & Exception_Information(Error));
   -- END ListenForThrottleTaskType;

   -----------------------------------------------------
   -- connects to LocoBuffer package server or simulator
   -- stores socket in socketlist, end task
   -----------------------------------------------------
   TASK BODY ConnectToSimulatorOrLocoBufferTaskType IS
      ConnectSocket        : socketType;
   BEGIN
      initialize39DLL;
      
      IF Simulator THEN
         put_line("MessageIO -- in ConnectToSimulatorOrLocoBufferTask: trying to connect to train simulator");
         connectToServer(connectSocket, IpStrAda, SimulatorPort, 1.0, false); 
         put_line("MessageIO -- in ConnectToSimulatorOrLocoBufferTask: connected to train simulator");
      ELSE
         put_line("MessageIO -- in ConnectToSimulatorOrLocoBufferTask: trying to connect to loco buffer");
         connectToServer(connectSocket, IpStrAda, LocoBufferPort, 1.0, false); 
         put_line("MessageIO -- in ConnectToSimulatorOrLocoBufferTask: connected to LocoBuffer pkg server");
      END IF;
      
      SocketList.AddRailroadSocket(ConnectSocket);
   EXCEPTION
      WHEN error: OTHERS =>
         put_line("**************** EXCEPTION in MessageIO: ConnectToSimulatorOrLocoBufferTaskType " & Exception_Information(Error));
   END ConnectToSimulatorOrLocoBufferTaskType;

   -- TASK BODY ConnectToSimulatorOrLocoBufferTaskType IS
      -- ConnectSocket, CValue        : C.Double;
   -- BEGIN
      -- CValue := DllInit;
      
      -- LOOP
         -- IF Simulator THEN
            -- put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: trying to connect to train simulator");
            -- ConnectSocket := TcpConnect(
               -- Ip   => New_String (to_string(IpStrAda)),
               -- Port => SimulatorPort,
               -- Mode => C.Double (1));            -- Does this cause nonblocking on SendMessage????
         -- ELSE
            -- put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: trying to connect to loco buffer");
            -- ConnectSocket := TcpConnect(
               -- Ip=>New_String(to_string(IpStrAda)), 
               -- Port=> LocoBufferPort, 
               -- Mode=>C.Double(1));               -- Does this cause nonblocking on SendMessage????
         -- END IF;
         -- EXIT WHEN Integer(ConnectSocket) > 0;
         -- put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: error when trying to connect");
      -- END LOOP;
      
      -- IF Simulator THEN
         -- put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: connected to train simulator");
      -- ELSE
         -- put_line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: connected to LocoBuffer pkg server");
      -- END IF;
      
      -- SocketList.AddRailroadSocket(ConnectSocket);
   -- EXCEPTION
      -- WHEN error: OTHERS =>
         -- put_line("**************** EXCEPTION in MessageIO: ConnectToSimulatorOrLocoBufferTaskType " & Exception_Information(Error));
   -- END ConnectToSimulatorOrLocoBufferTaskType;

   ---------------------------------------------------------
   -- get message from CommandQueueManager
   -- convert internal message into LocoNet Message
   -- send message to relevant sockets
   ---------------------------------------------------------
   TASK BODY SendMessageTaskType IS
      internalMessage : MessageType;
      MyArray         : ByteArrayType;
      Socket          : socketType;
      -- Socket          : C.Double;
      Slot            : SlotType;
      Size,
      SocketListLen   : Integer;
      messageCount    : natural := 0;
      turnoutId       : switchIdType;
      firstByte       : unsigned_8;
      secondByte      : unsigned_8;
      msgSize         : integer;
      -- CZero : C.Double := C.Double (0);
   BEGIN
      loop
         SocketList.GetSocket(0, Socket);
         exit when integer(socket) > 0;
         delay 1.0;  -- no point in checking for connection to locobuffer/simulator too frequently
      end loop;
      
      LOOP
         BEGIN
            CommandQueueManager.Get(InternalMessage);
            
            -- Simplify code by using the shorter names "MyArray" and "msgSize"
            MyArray := internalMessage.ByteArray;
            msgSize := internalMessage.size;
            
            -- Change train ids to slot numbers
            firstByte := MyArray(1);
            secondByte := MyArray(2);
            case firstByte is
               when OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
                  Slot := SlotLookupTable.TrainIdToPhysSlotNum(Integer(MyArray(2)));
                  MyArray(2) := Unsigned_8(Slot);
                  MyArray(msgSize) := makeChecksumByte(MyArray, msgSize);                
               when OPC_SL_RD_DATA =>
                  -- Slot := SlotLookupTable.TrainIdToVirtSlotNum(Integer(MyArray(3)));
                  -- MyArray(3) := Unsigned_8(Slot);
                  MyArray(msgSize) := makeChecksumByte(MyArray, msgSize);  
               when UZERO =>
                  case secondByte is
                     when PutTrainState | PutTrainPosition | PutTrainInformation =>
                        Slot := SlotLookupTable.TrainIdToVirtSlotNum( Integer(MyArray(3)));
                        MyArray(3) := Unsigned_8(Slot);               
                     when Others =>
                        null;
                  end case;
               when OTHERS =>
                  null;
            end case;
         
            -- Save changes to myArray
            internalMessage.byteArray := myArray;
            
            -- Echo the message
            if firstByte /= uzero then
                messageCount := messageCount + 1;
                myPutLine("<" & natural'image(messageCount) & " " & toEnglish(internalMessage) & " ...MesIOPkg.SendMesTask to outside");      
            else
                myPutLine("< " & toEnglish(internalMessage) & " ...MesIOPkg.SendMesTask to outside");      
            end if;         
            myPutLine(" ");
         
            -- Clear the message output buffer
            -- CValue := ClearBuffer(CZero);--clear buffer
            
            -- Send the messages
            CASE firstByte IS         
               WHEN OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND | OPC_SW_STATE |
                    OPC_LOCO_ADR | OPC_MOVE_SLOTS | OPC_WR_SL_DATA | OPC_GPON | 
                    OPC_GPOFF | OPC_SW_REQ =>                                               -- send to railroad 
            
                  -- Slot := SlotLookupTable.TrainIdToPhysSlotNum(Integer(MyArray(2)));
                  -- MyArray(2) := Unsigned_8(Slot);
                  -- MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size); 
                  SocketList.GetSocket(0, Socket);
                  sendMessage(socket, internalMessage, size);
                  -- FOR I IN 1..msgSize LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
                  
                  -- internalMessage.byteArray := myArray;
                  LastMessageManager.saveMessage(internalMessage);
                  
                  -- Extra processing for OPC_SW_STATE
                  if firstByte = OPC_SW_STATE then
                     -- Save the id of the turnout in a queue so that it can be matched
                     -- to the correct Long Ack which will be reporting the state of the turnout
                     splitSwStateMsg(internalMessage, turnoutId);
                     turnoutIdQueue.putId(turnoutId);
                                 
                     -- When the layout manager is processing the XML layout file, it puts
                     -- a group of these instructions on the output queue in rapid succession.
                     -- If these instructions are sent by TCP/IP to the locobuffer in rapid
                     -- rapid succession is there any chance some will be lost before then
                     -- locobuffer processes them? Probably not, but if so, we can put in a delay
                     -- here.
                     if not simulator then
                        null;
                        --delay 3.0;   -- to avoid overwhelming the locobuffer
                     end if;
                  end if;
              
                  -- Extra processing for power on and off instructions
                  if myArray(1) = OPC_GPON or myArray(1) = OPC_GPOFF then
                     lastMessageManager.enterPowerChangeMode;
                  end if;                                    
           
               WHEN OPC_SL_RD_DATA | OPC_LONG_ACK | OPC_SW_REP =>             --send to all othrottles, not railroad
            
                  SocketList.GetSocketListLength(SocketListLen);
                  FOR I IN 1..(SocketListLen-1) LOOP
                     SocketList.GetSocket(I, Socket);
                     IF Integer(Socket) > 0 THEN
                        sendMessage(socket, internalMessage, size);
                     END IF;
                  END LOOP;
                  -- FOR I IN 1..msgSize LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- SocketList.GetSocketListLength(SocketListLen);
                  -- FOR I IN 1..(SocketListLen-1) LOOP
                     -- SocketList.GetSocket(I, Socket);
                     -- IF Integer(Socket) > 0 THEN
                        -- Size := Integer(SendMessage(Socket, New_String(""),CZero, CZero));
                     -- END IF;
                  -- END LOOP;

              
               WHEN UZero =>   --extended messages send to all othrottles
            
                  SocketList.GetSocketListLength(SocketListLen);
                  FOR I IN 1..(SocketListLen-1) LOOP
                     SocketList.GetSocket(I, Socket);
                     IF Integer(Socket) > 0 THEN
                        sendMessage(socket, internalMessage, size);
                     END IF;
                  END LOOP;
                 -- FOR I IN 1..msgSize LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- SocketList.GetSocketListLength(SocketListLen);
                  -- FOR I IN 1..(SocketListLen-1) LOOP
                     -- SocketList.GetSocket(I, Socket);
                     -- IF Integer(Socket) > 0 THEN
                        -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
                     -- END IF;
                  -- END LOOP;
               WHEN OTHERS=>
                  null;
            END CASE;
         
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
      Length             : Integer;
      Socket             : socketType;
      -- Socket             : C.Double;
      -- CValue             : C.Double;
      Size               : Integer;
      MyArray            : ByteArrayType; --array of unsigned_8
      internalMessage    : MessageType;
      TrainId            : TrainIdType;
      messageCount       : natural := 0;
      found              : boolean;
      messagesEqual      : boolean;
      ignoringMessages   : boolean := false;
      answer             : boolean;
      switchId           : switchIdType;
      responseToOpcode    : unsigned_8;
      switchState          : switchStateType;
      -- CZero : C.Double := C.Double (0);      
   BEGIN
      loop
         SocketList.GetSocket(0, Socket);
         exit when integer(socket) > 0;
         delay 0.1;  -- no point in checking for connection to locobuffer/simulator too frequently
      end loop;

      LOOP
         BEGIN
            SocketList.GetSocketListLength(Length);
            FOR I IN 0..(Length-1) LOOP
               --read from each socket in socketlist
               SocketList.GetSocket(I, Socket);
               lastMessageManager.powerChangeCompletingNow(answer);
               if answer then
                  commandQueueManager.put(makePutPowerChangeCompleteMsg);
               end if;
               
               IF (Integer(Socket) = -1) THEN
                  null;
               else -- read from client
                  
                  receiveMessage(socket, internalMessage, size);
                  myArray := internalMessage.byteArray;
                  -- CValue := ClearBuffer(CZero);
                  -- Size := Integer(ReceiveMessage(Socket, CZero, CZero));
                  IF Size <= 0 THEN
                     null;
                  else
                  
                     lastMessageManager.messageReceived;
                  
                     -- FOR J IN 1..Size LOOP
                        -- MyArray(J) := Unsigned_8(ReadByte(CZero));
                     -- END LOOP;

                     -- Echo the message
                     -- internalMessage.Size := natural(Size);
                     -- internalMessage.ByteArray := MyArray;
                     myPutLine(" ");
                     if i /= 0 then
                        -- not from simulator/locobuffer server
                        myPutLine("> " & toEnglish(internalMessage) & " ... MesIOPkg.RecMesTask");
                     else
                        -- from simulator/locobuffer server
                        messageCount := messageCount + 1;
                        myPutLine(">" & natural'image(messageCount) & " " & toEnglish(internalMessage) & " ... MesIOPkg.RecMesTask");
                     end if;
                     
                     ignoringMessages := lastMessageManager.inPowerChangeMode;
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
                           WHEN OPC_LONG_ACK =>       -- Discard if not from railroad/simulator
                              if I /= 0 then
                                 myPutLine("       ... ignored because not from simulator/locobuffer");
                              else
                                 splitLongAck(internalMessage, responseToOpcode, switchState);
                                 if responseToOpcode /= OPC_SW_STATE then
                                    CommandQueueManager.put(InternalMessage);
                                 else
                                    if turnoutIdQueue.isEmpty then
                                       myPutLine("       !!! IGNORED, VERY BAD, couldn't find matching OP_SW_STATE message !!!");
                                    else   
                                       turnoutIdQueue.getId(switchId); 
                                       CommandQueueManager.put(makeSwRepMsg(switchId, switchState));
                                    end if;
                                 end if;
                              end if;
                           WHEN OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
                              SlotLookupTable.VirtSlotNumToTrainId(Integer(MyArray(2)), trainId, found);
                              if found then
                                 internalMessage.ByteArray(2) := Unsigned_8(TrainId);
                                 CommandQueueManager.put(InternalMessage);
                              else  
                                 myPutLine("       ... message ignored because not using an established virtual slot number ");
                              end if;
                           WHEN OPC_LOCO_ADR | OPC_MOVE_SLOTS | OPC_SW_REQ | OPC_WR_SL_DATA | OPC_SW_STATE =>                          -- discard if from railroad/simulator
                              IF I = 0 THEN
                                 myPutLine("       ... ignored because from simulator/locobuffer");
                              else
                                CommandQueueManager.put(InternalMessage);
                              END IF;
                           WHEN OPC_SL_RD_DATA =>
                              CommandQueueManager.put(InternalMessage);
                           WHEN OPC_INPUT_REP =>
                              declare
                                 sensor  : positive;
                                 isHi    : boolean;
                              begin
                                 splitInputRepMsg(internalMessage, sensor, isHi);
                                 if isHi then
                                    myPutLine("       ... ignored because input hi");
                                 else
                                    CommandQueueManager.put(InternalMessage);
                                 end if;
                              end;
                           WHEN OTHERS =>
                              if myArray(1) = 0 and myArray(2) = getTrainPosition then
                                 SlotLookupTable.VirtSlotNumToTrainId(Integer(MyArray(3)), trainId, found);
                                 if found then
                                    internalMessage.ByteArray(3) := Unsigned_8(TrainId);
                                    CommandQueueManager.put(InternalMessage);
                                 else  
                                    myPutLine("       ... message ignored because not using an established virtual slot number ");
                                 end if;
                              else
                                 CommandQueueManager.put(InternalMessage);
                              end if;
                        END CASE;
                     end if;
                  end if;
               END IF;
            END LOOP;
            DELAY 0.001; -- no point in checking for incoming messages too frequently
         EXCEPTION
            WHEN Error : OTHERS =>
               put_line("**************** EXCEPTION in MessageIO: ReceiveMessageTask ( " & Exception_Information(Error) & " )");
         END;
      END LOOP;
   END ReceiveMessageTaskType;
   
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
   
   PROTECTED BODY TurnoutIdQueueType IS                                     

      procedure putId(id : IN switchIdType) IS
      BEGIN
         TurnoutIdQueuePkg.Enqueue(Queue, id);
         count := count + 1;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION MessageIO putId " & Exception_Information(Error));
            raise;
      END putId;

      entry GetId(id : OUT switchIdType) when count > 0 IS
      BEGIN
         TurnoutIdQueuePkg.Dequeue(Queue, id);
         count := count - 1;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION MessageIO GetId " & Exception_Information(Error));
            raise;
      END GetId;

      function IsEmpty return boolean IS
      BEGIN
         return TurnoutIdQueuePkg.IsEmpty(Queue);
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION MessageIO IsEmpty " & Exception_Information(Error));
            raise;
      END IsEmpty;

   END TurnoutIdQueueType;


END MessageIO;

