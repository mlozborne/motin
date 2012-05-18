--MessageIO package body
--4/8/2011
WITH Interfaces.C.Strings;
USE Interfaces.C.Strings;
WITH Interfaces;
USE Interfaces;
WITH Ada.Text_IO;
USE Ada.Text_IO;
WITH Api39dll;
USE Api39dll;
WITH CommandQueueManager;
USE CommandQueueManager;
WITH Globals;
USE Globals;
WITH Ada.Exceptions;
USE  Ada.Exceptions;
with MessageTranslationLibrary; use MessageTranslationLibrary;  -- mo 1/7/12
with MessageTranslationTypes; use messageTranslationTypes;


PACKAGE BODY MessageIO IS

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
         Put_Line("MessageIO pkg in ListenForThrottleTask: setting up server for OThrottles at 1235");
         ListenSocket := TcpListen(
            Port => MessageIOPort,
            Max  => C.Double (2),
            Mode => C.Double (0));             -- 0 here causes blocking on TcpAccept
         EXIT WHEN Integer(ListenSocket) > 0;
         Put_Line("MessageIO pkg in ListenForThrottleTask: error in setting up server for OThrottles");
      END LOOP;
      Put_Line("MessageIO pkg in ListenForThrottleTask: server for OThrottles now set up");
      
      LOOP--loop to add clients
         LOOP--loop to accept connections
            Put_Line("MessageIO pkg in ListenForThrottleTask: listening for OThrottle");
            AcceptSocket := TcpAccept(Sockid => ListenSocket, Mode => C.Double (1));    -- 1 here causes nonblocking on ReceiveMessage
            EXIT WHEN Integer(AcceptSocket) > 0;
            Put_Line("MessageIO pkg in ListenForThrottleTask: error in listening for OThrottle");
            --DELAY 1.0;                 -- test 6                        
         END LOOP;
         Put_Line("MessageIO pkg in ListenForThrottleTask: new OThrottle at socket " & C.Double'Image(AcceptSocket));
         SocketList.AddSocket(AcceptSocket);--add to socketlist
      END LOOP;
   EXCEPTION
      WHEN error: OTHERS =>
         Put("**************** EXCEPTION in MessageIO: ListenForThrottle " & Exception_Information(Error));
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
            Put_Line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: trying to connect to train simulator");
            ConnectSocket := TcpConnect(
               Ip   => New_String ("127.0.0.1"),
               Port => SimulatorPort,
               Mode => C.Double (1));            -- Does this cause nonblocking on SendMessage????
         ELSE
            Put_Line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: trying to connect to loco buffer");
            ConnectSocket := TcpConnect(
               Ip=>New_String("127.0.0.1"), 
               Port=> LocoBufferPort, 
               Mode=>C.Double(1));               -- Does this cause nonblocking on SendMessage????
         END IF;
         EXIT WHEN Integer(ConnectSocket) > 0;
         Put_Line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: error when trying to connect");
      END LOOP;
      
      IF Simulator THEN
         Put_Line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: connected to train simulator");
      ELSE
         Put_Line("MessageIO pkg in ConnectToSimulatorOrLocoBufferTask: connected to LocoBuffer pkg server");
      END IF;
      
      SocketList.AddRailroadSocket(ConnectSocket);
   EXCEPTION
      WHEN error: OTHERS =>
         Put("**************** EXCEPTION in MessageIO: ConnectToRailroad " & Exception_Information(Error));
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
   BEGIN
      LOOP
         BEGIN
            CommandQueueManager.Get(InternalMessage);
            put_line(toEnglish(internalMessage) & " ...MesIOPkg.SendMesTask to outside");      
            MyArray := internalMessage.ByteArray;
            CValue := ClearBuffer(CZero);--clear buffer
            CASE MyArray(1) IS
               WHEN OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
                  --slot lookup table, send to railroad
                  -- Put_Line("OPC_LOCO_SPD train " &  Integer'Image(Integer(MyArray(2))) & "     MessageIOPkg.SendMesageTask to railroad");
                  Slot := SlotLookupTable.TrainIdToPhysSlotNum(Integer(MyArray(2)));
                  MyArray(2) := Unsigned_8(Slot);
                  -- Put_Line("                                 physical slot number is: " & Integer'Image(Slot));
                  MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                  SocketList.GetSocket(0, Socket);
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               -- WHEN OPC_LOCO_DIRF  =>
                  --slot lookup table, send to railroad
                  --Put_Line("OPC_LOCO_DIRF train " &  Integer'Image(Integer(MyArray(2))) & "     MessageIOPkg.SendMesageTask to railroad");
                  -- Slot := SlotLookupTable.TrainIdToPhysSlotNum(Integer(MyArray(2)));
                  -- MyArray(2) := Unsigned_8(Slot);
                  -- MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               -- WHEN OPC_LOCO_SND =>
                  --slot lookup table, send to railroad
                  --Put_Line("OPC_LOCO_SND train " &  Integer'Image(Integer(MyArray(2))) & "     MessageIOPkg.SendMesageTask to railroad");
                  -- Slot := SlotLookupTable.TrainIdToPhysSlotNum(Integer(MyArray(2)));
                  -- MyArray(2) := Unsigned_8(Slot);
                  -- MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               WHEN OPC_LOCO_ADR | OPC_MOVE_SLOTS | OPC_WR_SL_DATA | OPC_GPON | OPC_GPOFF | OPC_SW_REQ =>--send to railroad slot
                  --third byte is 'address'???
                  -- Put_Line("OPC_LOCO_ADR              MessageIOPkg.SendMesageTask to railroad");
                  SocketList.GetSocket(0, Socket);
                  FOR I IN 1..internalMessage.Size LOOP
                     CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  END LOOP;
                  Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               --WHEN OPC_GPON =>                                      -- mo 12/14/11
                  -- Put_Line("OPC_GPON                  MessageIOPkg.SendMesageTask to railroad");
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               --WHEN OPC_GPOFF =>                                      -- mo 12/14/11
                  -- Put_Line("OPC_GPOFF                 MessageIOPkg.SendMesageTask to railroad");
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               --WHEN OPC_SW_REQ =>
                  -- Put_Line("OPC_SW_REQ                MessageIOPkg.SendMesageTask to railroad");
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               --WHEN OPC_MOVE_SLOTS =>
                  -- slot lookup table for 2 slots, send to railroad slot
                  -- Put_Line("OPC_MOVE_SLOTS            MessageIOPkg.SendMesageTask to railroad");
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               --WHEN OPC_WR_SL_DATA =>
                  -- send to railroad slot
                  -- Put_Line("OPC_WR_SL_DATA            MessageIOPkg.SendMesageTask to railroad");
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
               WHEN OPC_SL_RD_DATA =>
                  --send to all throttles, not railroad
                  --third byte is slot#
                  -- Put_Line("OPC_SL_RD_DATA            MessageIOPkg.SendMesageTask to all throttles");
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
               WHEN OPC_LONG_ACK | OPC_SW_REP =>--send to all throttles, not railroad
                  -- Put_Line("OPC_LONG_ACK              MessageIOPkg.SendMesageTask to all throttles");
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
               --WHEN OPC_SW_REP =>
                  -- Put_Line("OPC_SW_REP                MessageIOPkg.SendMesageTask to all throttles");
                  -- SocketList.GetSocket(0, Socket);
                  -- FOR I IN 1..internalMessage.Size LOOP
                     -- CValue := WriteByte(C.Double(MyArray(I)), CZero);
                  -- END LOOP;
                  -- SocketList.GetSocketListLength(SocketListLen);
                  -- FOR I IN 1..(SocketListLen-1) LOOP
                     -- SocketList.GetSocket(I, Socket);
                     -- IF Integer(Socket) > 0 THEN
                        -- Size := Integer(SendMessage(Socket, New_String(""), CZero, CZero));
                     -- END IF;
                  -- END LOOP;
               WHEN UZero =>--extended messages
                  CASE MyArray(2) IS
                     WHEN PutTrainState | PutTrainPosition | PutTrainInformation =>
                        --third byte is slot num, send to all throttles
                        -- Put_Line("PutTrainState             MessageIOPkg.SendMesageTask to all OThrottles");
                        Slot := SlotLookupTable.TrainIdToVirtSlotNum( Integer(MyArray(3)));
                        MyArray(3) := Unsigned_8(Slot);
                        -- MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                     --WHEN PutTrainPosition =>
                        --third byte is slot num, send to all throttles
                        -- Put_Line("PutTrainPosition          MessageIOPkg.SendMesageTask to all OThrottles");
                        -- Slot := SlotLookupTable.TrainIdToVirtSlotNum(Integer(MyArray(3)));
                        -- MyArray(3) := Unsigned_8(Slot);
                        -- MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
                     -- WHEN PutSectionState =>--send to all throttles
                        -- Put_Line("PutSectionStat            MessageIOPkg.SendMesageTask to all OThrottles");
                     -- WHEN PutSwitchState =>--send to all throttles
                        -- Put_Line("PutSwitchState            MessageIOPkg.SendMesageTask to all OThrottles");
                     -- WHEN PutSensorState =>--send to all throttles
                        -- Put_Line("PutSensorState            MessageIOPkg.SendMesageTask to all OThrottles");
                     -- WHEN PutInitOutcome =>
                        -- no idea what to do with slot nums, send to all throttles
                        -- Put_Line("PutInitOutcome            MessageIOPkg.SendMesageTask to all OThrottles");
                     -- WHEN PutReadLayoutResponse =>--send to all throttles
                        -- Put_Line("PutReadLayoutResponse     MessageIOPkg.SendMesageTask to all OThrottles");
                     --WHEN PutTrainInformation =>
                        --third byte is slot num, send to all throttles
                        -- Put_Line("PutTrainInformation       MessageIOPkg.SendMesageTask to all OThrottles");
                        -- Slot := SlotLookupTable.TrainIdToVirtSlotNum(Integer(MyArray(3)));
                        -- MyArray(3) := Unsigned_8(Slot);
                        -- MyArray(internalMessage.size) := makeChecksumByte(myArray, internalMessage.size);  -- mo 1/7/12
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
                  Put_Line("???Unidentified???  MessageIOPkg.SendMesageTask to railroad");
            END CASE;
            new_line;
            --DELAY 0.01; --0.1;            test 5  necessary here                                  -- mo 12/17/11
         EXCEPTION
            WHEN Error : OTHERS =>
               Put_Line("**************** EXCEPTION in MessageIO: SendMessageTask ( " & Exception_Information(Error) & " )");
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
   BEGIN
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

                     internalMessage.Size := Natural(Size);
                     internalMessage.ByteArray := MyArray;
                     new_line;
                     put_line(toEnglish(internalMessage) & " ...in MesIOPkg.RecMesTask");
                     CASE MyArray(1) IS
                        WHEN OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
                           -- Put_Line("OPC_LOCO_SPD        MessageIOPkg.ReceiveMessageTask from throttle" );
                           TrainId := SlotLookupTable.VirtSlotNumToTrainId(Integer(MyArray(2)));
                           internalMessage.ByteArray(2) := Unsigned_8(TrainId);
                           --MyArray(2) := Unsigned_8(TrainId);
                           --internalMessage.ByteArray := MyArray;
                           CommandQueueManager.Put(InternalMessage);
                        --WHEN OPC_LOCO_DIRF =>
                           -- Put_Line("OPC_LOCO_DIRF       MessageIOPkg.ReceiveMessageTask from throttle" );
                           -- TrainId := SlotLookupTable.VirtSlotNumToTrainId(Integer(MyArray(2)));
                           -- MyArray(2) := Unsigned_8(TrainId);
                           -- internalMessage.ByteArray := MyArray;
                           -- CommandQueueManager.Put(InternalMessage);
                        --WHEN OPC_LOCO_SND =>
                           -- Put_Line("OPC_LOCO_SND        MessageIOPkg.ReceiveMessageTask from throttle" );
                           -- TrainId := SlotLookupTable.VirtSlotNumToTrainId(Integer(MyArray(2)));
                           -- MyArray(2) := Unsigned_8(TrainId);
                           -- internalMessage.ByteArray := MyArray;
                           -- CommandQueueManager.Put(InternalMessage);
                        WHEN OPC_LOCO_ADR | OPC_MOVE_SLOTS | OPC_SW_REQ =>                          -- discard if from railroad/simulator
                           IF I /= 0 THEN
                              --Put_Line("OPC_LOCO_ADR        MessageIOPkg.ReceiveMessageTask from throttle" );
                              --internalMessage.ByteArray := MyArray;
                              CommandQueueManager.Put(InternalMessage);
                           END IF;
                        --WHEN OPC_SW_REQ =>
                           --IF I /= 0 THEN
                              --Put_Line("OPC_SW_REQ          MessageIOPkg.ReceiveMessageTask from throttle" );
                              -- internalMessage.ByteArray := MyArray;
                              -- CommandQueueManager.Put(InternalMessage);
                           -- END IF;
                        --WHEN OPC_MOVE_SLOTS =>                       -- discard if from railroad/simulator   -- mo 1/15/12
                           --if i /= 0 then
                              --Put_Line("OPC_MOVE_SLOTS      MessageIOPkg.ReceiveMessageTask from throttle" );
                              -- internalMessage.ByteArray := MyArray;      -- mo 1/15/12
                              -- CommandQueueManager.Put(InternalMessage);
                           -- end if;
                        WHEN OPC_LONG_ACK =>                        -- Discard if not from railroad/simulator
                           IF I = 0 THEN
                              --Put_Line("OPC_LONG_ACK        MessageIOPkg.ReceiveMessageTask from railroad/simulator" );
                              --internalMessage.ByteArray := MyArray;
                              CommandQueueManager.Put(InternalMessage);
                           END IF;
                        WHEN OPC_SL_RD_DATA =>
                           --third byte is slot num, from railroad
                           --TrainId := SlotLookupTable.PhysSlotNumToTrainId(Integer(MyArray(3)));
                           --internalMessage.ByteArray(3) := Unsigned_8(TrainId);
                           --MyArray(3) := Unsigned_8(TrainId);
                           --Put_Line("OPC_SL_RD_DATA         MessageIOPkg.ReceiveMessageTask from railroad/simulator" );
                           --internalMessage.ByteArray := MyArray;
                           CommandQueueManager.Put(InternalMessage);
                        WHEN OTHERS =>
                           --GetSwitchSuccessor, DoLocoInit, doReadLayout, GetSwitchStates, OPC_GPON, OPC_GPOFF, OPC_SW_REQ, OPC_SW_REP, OPC_INPUT_REP
                           --Put_Line("Other message          MessageIOPkg.ReceiveMessageTask from throttle" );
                           --internalMessage.ByteArray := MyArray;
                           CommandQueueManager.Put(InternalMessage);
                     END CASE;

                  END IF;

               END IF;
            END LOOP;
            DELAY 0.01; --0.1;                         -- mo 12/17/11   test 4 necessary here
         EXCEPTION
            WHEN Error : OTHERS =>
               Put_Line("**************** EXCEPTION in MessageIO: ReceiveMessageTask ( " & Exception_Information(Error) & " )");
         END;
      END LOOP;
   END ReceiveMessageTaskType;

END MessageIO;

