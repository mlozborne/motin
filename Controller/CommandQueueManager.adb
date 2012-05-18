-- Message Queue Manager Package body
--4/8/2011
WITH Ada.Text_IO, Ada.Exceptions;
USE Ada.Text_IO, Ada.Exceptions;
WITH MessageTranslationLibrary; USE MessageTranslationLibrary;
-- mo 12/16/11

PACKAGE BODY CommandQueueManager IS

   PROCEDURE Put (message : IN MessageType) IS
      TrainQueuePtr : QueuePtr;

   BEGIN
      --sort into correct list
      CASE message.ByteArray(1) IS
         when OPC_GPON | OPC_GPOFF =>
            put_line("  " & toEnglish(message) & "            ComQuMngr.Put to out queue... ");
            CommandQueueManager.OutQueue.putMessage(message);
         -- WHEN OPC_GPON =>                                       -- mo 12/14/11
            -- Put_Line("  OPC_GPON      ComQuMngr.Put to out queue... ");
            -- CommandQueueManager.OutQueue.putMessage(makePowerOnMsg);
         -- WHEN OPC_GPOFF =>                                      -- mo 12/14/11
            -- Put_Line("  OPC_GPOFF     ComQuMngr.Put to out queue... ");
            -- CommandQueueManager.OutQueue.putMessage(makePowerOffMsg);
            
         when OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
            put_line("  " & toEnglish(message) & "            ComQuMngr.Put to train queue... ");
            TrainIdQueueList.GetQueue(Integer(message.ByteArray(2)), TrainQueuePtr);
            TrainQueuePtr.putMessage(Message);         
         -- WHEN OPC_LOCO_SPD =>--send to train queue
            -- Put_Line("  OPC_LOCO_SPD  ComQuMngr.Put to train queue... ");
            -- TrainIdQueueList.GetQueue(Integer(message.ByteArray(2)), TrainQueuePtr);
            -- TrainQueuePtr.putMessage(Message);
         -- WHEN OPC_LOCO_DIRF =>--send to train queue
            -- Put_Line("  OPC_LOCO_DIRF ComQuMngr.Put to train queue... ");
            -- TrainIdQueueList.GetQueue(Integer(message.ByteArray(2)), TrainQueuePtr);
            -- TrainQueuePtr.putMessage(Message);
         -- WHEN OPC_LOCO_SND =>--send to train queue
            -- Put_Line("  OPC_LOCO_SND  ComQuMngr.Put to train queue... ");
            -- TrainIdQueueList.GetQueue(Integer(message.ByteArray(2)), TrainQueuePtr);
            -- TrainQueuePtr.putMessage(Message);
            
         when OPC_SW_REQ | OPC_SW_REP | OPC_INPUT_REP =>
            put_line("  " & toEnglish(message) & "            ComQuMngr.Put to layout queue... ");
            LayoutQueue.putMessage(Message);
         -- WHEN OPC_SW_REQ =>--send to layout queue
            -- Put_Line("  OPC_SW_REQ    ComQuMngr.Put to layout queue... ");
            -- LayoutQueue.putMessage(Message);
         -- WHEN OPC_SW_REP =>--send to layout queue
            -- Put_Line("  OPC_SW_REP    ComQuMngr.Put to layout queue... ");
            -- LayoutQueue.putMessage(Message);
         -- WHEN OPC_INPUT_REP => --send to Layout queue
            -- Put_Line("  OPC_INPUT_REP ComQuMngr.Put to layout queue... ");
            -- LayoutQueue.putMessage(Message);
            
         when OPC_LOCO_ADR | OPC_SL_RD_DATA | OPC_LONG_ACK =>
            put_line("  " & toEnglish(message) & "            ComQuMngr.Put to SSI queue... ");
            SSIQueue.putMessage(Message);
         -- WHEN OPC_LOCO_ADR =>--send to SSIQueue
            -- Put_Line("  OPC_LOCO_ADR  ComQuMngr.Put to SSI queue... ");
            -- SSIQueue.putMessage(Message);
         -- WHEN OPC_SL_RD_DATA =>--send to SSI Queue
            -- Put_Line("  OPC_SL_RD_DATA ComQuMngr.Put to SSI queue... ");
            -- SSIQueue.putMessage(Message);
         -- WHEN OPC_LONG_ACK=>--send to SSI queue
            -- Put_Line("  OPC_LONG_ACK   CommandQueueManager.Putto SSI queue... ");
            -- SSIQueue.putMessage(Message);
            
         WHEN UZero =>
         
            CASE message.ByteArray(2) IS
            
               when MsgTrainTaskQuit | MsgReinitializeTrain =>
                  put_line("  " & toEnglish(message) & "            ComQuMngr.Put to train queue... ");
                  TrainIdQueueList.GetQueue(Integer(message.ByteArray(3)), TrainQueuePtr);
                  TrainQueuePtr.putMessage(Message);                             
               -- when MsgTrainTaskQuit => -- send to train queue i           -- mo 1/28/12
                  -- Put_Line("  MsgTrainTaskQuit      ComQuMngr.Put to train queue... ");
                  -- TrainIdQueueList.GetQueue(Integer(message.ByteArray(3)), TrainQueuePtr);
                  -- TrainQueuePtr.putMessage(Message);                             
               -- when MsgReinitializeTrain => -- send to train queue i           -- mo 1/28/12
                  -- Put_Line("  MsgReinitializeTrain  ComQuMngr.Put to train queue... ");
                  -- TrainIdQueueList.GetQueue(Integer(message.ByteArray(3)), TrainQueuePtr);
                  -- TrainQueuePtr.putMessage(Message);              
               
               WHEN DoLocoInit=>--send to SSI queue
                  put_line("  " & toEnglish(message) & "            ComQuMngr.Put to SSI queue... ");
                  -- Put_Line("  DoLocoInit            ComQuMngr.Put to SSI queue... ");
                  SSIQueue.putMessage(Message);
                
               when GetSwitchSuccessor | DoReadLayout | GetSwitchStates =>
                  put_line("  " & toEnglish(message) & "            ComQuMngr.Put to layout queue... ");
                  LayoutQueue.putMessage(Message);
               -- WHEN GetSwitchSuccessor=>--send to layout queue
                  -- Put_Line("  GetSwitchSucessor    ComQuMngr.Put to layout queue... ");
                  -- LayoutQueue.putMessage(Message);
               -- WHEN DoReadLayout=>--send to layout queue
                  -- Put_Line("  DoReadLayout         ComQuMngr.Put to layout queue... ");
                  -- LayoutQueue.putMessage(Message);
               -- WHEN GetSwitchStates=>--send to layout queue
                  -- Put_Line("  GetSwitchStates      ComQuMngr.Put to layout queue... ");
                  -- LayoutQueue.putMessage(Message);
                  
               WHEN OTHERS =>
                  Put_Line("  Undefined Extended Message   ComQuMngr.Put: ");
            END CASE;
         WHEN OTHERS =>
            Put_Line("  Undefined Opcode     ComQuMngr.Put: ");
      END CASE;
      NULL;
   EXCEPTION
      WHEN error: OTHERS=>
         Put_Line("**************** EXCEPTION CommandQueueManager in Put: " & Exception_Information(Error));
         raise;
   END Put;

   PROCEDURE Get (message : OUT MessageType) IS
   BEGIN
      -- WHILE OutQueue.IsEmpty LOOP                      
         -- DELAY 0.01; --0.1;                          -- test 8  
      -- END LOOP;
      OutQueue.GetMessage(Message);
   EXCEPTION
      WHEN error: OTHERS=>
         Put_Line("**************** EXCEPTION CommandQueueManager: Get " & Exception_Information(Error));
         raise;
   END Get;


   PROTECTED BODY SocketListT IS

      PROCEDURE AddSocket (Socket : IN  C.Double) IS
         -- SocketListFull : EXCEPTION;
      BEGIN
         FOR I IN 1..(SocketListArray'Length-1) LOOP
            IF (Integer(SocketListArray(I)) = -1) THEN
               SocketListArray(I) := Socket;
               RETURN;
            END IF;
         END LOOP;
         RAISE SocketListFull;
      EXCEPTION
         -- WHEN SocketListFull =>
            -- Put_Line("**************** EXCEPTION in CommandQueueManager: SocketList (tried to add socket to full socketlist" & Exception_Information(SocketListFull));
            -- raise;
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SocketListT.AddSocket " & Exception_Information(Error));
            raise;
      END AddSocket;

      PROCEDURE AddRailroadSocket (Socket : IN C.Double) IS
      BEGIN
         SocketListArray(0) := Socket;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SocketListT.AddRailRoadSocket " & Exception_Information(Error));
            raise;
      END AddRailroadSocket;


      PROCEDURE GetSocketListLength (Length : OUT Integer) IS
      BEGIN
         Length := SocketListArray'Length;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SocketListT.GetSocketListLength " & Exception_Information(Error));
            raise;
      END GetSocketListLength;

      PROCEDURE GetSocket (Index : IN  Integer; Socket : OUT C.Double) IS
      BEGIN
         IF (Index >= 0) AND (Index < SocketListArray'Length) THEN
            Socket := SocketListArray(Index);
         END IF;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SocketListT.GetSocket " & Exception_Information(Error));
            raise;
      END GetSocket;

   END SocketListT;
--------------------------------------------------------------------------
   -- PROTECTED BODY MessageQueueType IS                                      -- mo 12/22/11

      -- PROCEDURE putMessage (message : IN     MessageType) IS
      -- BEGIN
         -- MessageQueue.Enqueue(Queue, Message);
      -- END putMessage;

      -- PROCEDURE GetMessage (message :    OUT MessageType) IS
      -- BEGIN
         -- MessageQueue.Dequeue(Queue, Message);
      -- END GetMessage;

      -- FUNCTION IsEmpty RETURN Boolean IS
      -- BEGIN
         -- RETURN MessageQueue.IsEmpty(Queue);
      -- END IsEmpty;

   -- END MessageQueueType;
--------------------------------------------------------------------------
   PROTECTED BODY MessageQueueType IS                                      -- mo 12/22/11

      procedure putMessage(message : IN MessageType) IS
      BEGIN
         MessageQueue.Enqueue(Queue, Message);
         count := count + 1;
      EXCEPTION
         WHEN error: OTHERS =>
            Put_Line("**************** EXCEPTION CommandQueueManager putMessage " & Exception_Information(Error));
            raise;
      END putMessage;

      entry GetMessage(message : OUT MessageType) when count > 0 IS
      BEGIN
         MessageQueue.Dequeue(Queue, Message);
         count := count - 1;
      EXCEPTION
         WHEN error: OTHERS =>
            Put_Line("**************** EXCEPTION CommandQueueManager GetMessage " & Exception_Information(Error));
            raise;
      END GetMessage;

      function IsEmpty return boolean IS
      BEGIN
         return MessageQueue.IsEmpty(Queue);
      EXCEPTION
         WHEN error: OTHERS =>
            Put_Line("**************** EXCEPTION CommandQueueManager IsEmpty " & Exception_Information(Error));
            raise;
      END IsEmpty;

   END MessageQueueType;
------------------------------------------------------------------------

   PROTECTED BODY SlotLookupTable IS


      procedure put is
         ent   : lookupEntry;
      begin
         new_line;
         put_line("        virtAddr virtSlot hasVirtSlot phyAddr phySlot hasPhySlot inUse");
         for i in lookupTable'range loop
            ent := lookupTable(I);
            put_line("        " & 
                     integer'image(ent.virtTrainAddr) &
                     integer'image(ent.virtSlotNum) &
                     boolean'image(ent.hasVirtSlot) &
                     integer'image(ent.physTrainAddr) &
                     integer'image(ent.physSlotNum) &
                     boolean'image(ent.hasPhySlot) &
                     boolean'image(ent.inuse));
         end loop;
         new_line;
      end put;

      ----------------------------------------------------------
      -- return index of first empty entry in lookup table
      ---------------------------------------------------------
      PROCEDURE RequestTrainId (TrainId : OUT TrainIdType) IS
         -- LookupTableFull : EXCEPTION;
         EntryFound : Boolean := False;
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).InUse = False THEN
               TrainId := I;
               EntryFound := True;
               LookupTable(I).InUse := True;
               EXIT;
            END IF;
         END LOOP;
         IF EntryFound = False THEN
            RAISE LookupTableFull;
         END IF;
      EXCEPTION
         -- WHEN LookupTableFull =>
            -- Put_Line("**************** EXCEPTION in CommandQueueManager: no empty entries in lookup table" & Exception_Information(LookupTableFull));
            -- raise;
         WHEN error: OTHERS =>
            Put_Line("**************** EXCEPTION CommandQueueManager: RequestTrainId " & Exception_Information(Error));
            raise;
      END RequestTrainId;
    
      procedure removeEntryByPhysAddr(PhysAddr : LocoAddressType) is  
      begin
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).InUse = true and lookupTable(I).physTrainAddr = PhysAddr THEN
               LookupTable(I).VirtTrainAddr := LocoAddressType'first;
               LookupTable(I).VirtSlotNum := SlotType'first;
               LookupTable(I).HasVirtSlot :=  False;
               LookupTable(I).PhysTrainAddr := LocoAddressType'first;
               LookupTable(I).PhysSlotNum := SlotType'first;
               LookupTable(I).HasPhySlot := False;
               LookupTable(I).InUse := False;
               if lookupTable(I).sensors /= null then
                  disposeSensorArray(LookupTable(I).Sensors); 
               end if;
               return;
            END IF;
         END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.removeEntryByPhysAddr " & Exception_Information(Error));
            raise;
      end removeEntryByPhysAddr;
      
      procedure removeEntryByTrainId(trainId : trainIdType) is 
         I : trainIdType := trainId;
      begin
         LookupTable(I).VirtTrainAddr := LocoAddressType'first;
         LookupTable(I).VirtSlotNum := SlotType'first;
         LookupTable(I).HasVirtSlot :=  False;
         LookupTable(I).PhysTrainAddr := LocoAddressType'first;
         LookupTable(I).PhysSlotNum := SlotType'first;
         LookupTable(I).HasPhySlot := False;
         LookupTable(I).InUse := False;
         if lookupTable(I).sensors /= null then
            disposeSensorArray(LookupTable(I).Sensors); 
         end if;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.removeEntryByTrainId " & Exception_Information(Error));
            raise;
      end removeEntryByTrainId;
      
      ---------------------------------------------------------------
      -- create new entry in lookup table
      -- TrainId should be same value returned by
      -- previous call to RequestTrainId ()
      ---------------------------------------------------------------
      PROCEDURE CreateEntry (
            VirtTrainAddr : IN     LocoAddressType;
            PhysTrainAddr : IN     LocoAddressType;
            TrainId       : IN     TrainIdType) IS
      BEGIN
         LookupTable(TrainId).PhysTrainAddr := PhysTrainAddr;
         LookupTable(TrainId).VirtTrainAddr := VirtTrainAddr;
         LookupTable(TrainId).InUse := True;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.CreateEntry " & Exception_Information(Error));
            raise;
      END CreateEntry;

      FUNCTION TrainIdToVirtSlotNum(TrainId: TrainIdType) RETURN  SlotType IS
         -- EntryNotFound: EXCEPTION;
      BEGIN
         RETURN LookupTable(TrainId).VirtSlotNum;
         -- IF LookupTable(TrainId).InUse THEN
            -- RETURN LookupTable(TrainId).VirtSlotNum;
         -- ELSE
            -- RAISE EntryNotFound;
         -- END IF;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager TrainIdToVirtSlotNum " & Exception_Information(Error));
            raise;
      END TrainIdToVirtSlotNum;

      FUNCTION TrainIdToPhysSlotNum(TrainId: TrainIdType) RETURN SlotType IS
         -- EntryNotFound: EXCEPTION;
      BEGIN
         RETURN LookupTable(TrainId).PhysSlotNum;
         -- IF Lookuptable(TrainId).InUse THEN
            -- RETURN LookupTable(TrainId).PhysSlotNum;
         -- ELSE
            -- RAISE EntryNotFound;
         -- END IF;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager TrainIdToPhysSlotNum " & Exception_Information(Error));
            raise;
      END TrainIdToPhysSlotNum;

      FUNCTION PhysSlotNumToTrainId(PhysSlotNum: SlotType) RETURN
            TrainIdType IS
         -- EntryNotFound :EXCEPTION;
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).PhysSlotNum = PhysSlotNum THEN
               RETURN I;
            END IF;
         END LOOP;
         RAISE EntryNotFound;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager PhysSlotNumToTrainId " & Exception_Information(Error));
            raise;
      END PhysSlotNumToTrainId;

      FUNCTION VirtSlotNumToTrainId(VirtSlotNum: SlotType) RETURN TrainIdType IS
         -- EntryNotFound: EXCEPTION;
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).VirtSlotNum = VirtSlotNum THEN
               RETURN I;
            END IF;
         END LOOP;
         RAISE EntryNotFound;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager VirtSlotNumToTrainId " & Exception_Information(Error));
            raise;
      END VirtSlotNumToTrainId;

      FUNCTION IsPhysAddrInTable(PhysAddr: LocoAddressType) RETURN Boolean IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).PhysTrainAddr = PhysAddr THEN
               RETURN True;
            END IF;
         END LOOP;
         RETURN False;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager IsPhysAddrInTable " & Exception_Information(Error));
            raise;
      END IsPhysAddrInTable;

      FUNCTION PhysAddrToTrainId(PhysAddr: LocoAddressType) RETURN TrainIdType IS
         -- EntryNotFound: EXCEPTION;
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).PhysTrainAddr = PhysAddr THEN
               RETURN I;
            END IF;
         END LOOP;
         RAISE EntryNotFound;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager PhysAddrToTrainId " & Exception_Information(Error));
            raise;
      END PhysAddrToTrainId;

      function addressToTrainId(address : natural) return slotType is  -- mo 1/14/12
      begin
         for i in lookupTable'range loop
            if lookupTable(i).hasVirtSlot and lookupTable(i).hasPhySlot and
            (lookupTable(i).virtTrainAddr = address or lookupTable(i).physSlotNum = address) then
               return i; 
            end if;
         end loop;
         return 0;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager addressToTrainId " & Exception_Information(Error));
            raise;
      end addressToTrainId;

      FUNCTION VirtAddrToTrainId(VirtAddr: LocoAddressType) RETURN
            TrainIdType IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).VirtTrainAddr = VirtAddr THEN
               RETURN I;
            END IF;
         END LOOP;
         RETURN 1;          
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager VirtAddrToTrainId " & Exception_Information(Error));
            raise;
      END VirtAddrToTrainId;

      FUNCTION TrainIdToVirtAddr(TrainId: TrainIdType) RETURN
            LocoAddressType IS
         -- EntryNotFound: EXCEPTION;
      BEGIN
         RETURN LookupTable(TrainId).VirtTrainAddr;
         -- IF LookupTable(TrainId).InUse THEN
            -- RETURN LookupTable(TrainId).VirtTrainAddr;
         -- ELSE
            -- RAISE EntryNotFound;
         -- END IF;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION CommandQueueManager TrainIdToVirAddr " & Exception_Information(Error));
            raise;
      END TrainIdToVirtAddr;

      FUNCTION TrainIdToPhysAddr(TrainId: TrainIdType) RETURN LocoAddressType IS
         -- EntryNotFound: EXCEPTION;
      BEGIN
         RETURN LookupTable(TrainId).PhysTrainAddr;
         -- IF LookupTable(TrainId).InUse THEN
            -- RETURN LookupTable(TrainId).PhysTrainAddr;
         -- ELSE
            -- RAISE EntryNotFound;
         -- END IF;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager TrainIdToPhysAddr " & Exception_Information(Error));
            raise;
      END TrainIdToPhysAddr;

      FUNCTION HasBothSlots(TrainId: TrainIdType) RETURN Boolean IS
      BEGIN
         RETURN LookupTable(TrainId).HasVirtSlot AND LookupTable(TrainId).HasPhySlot;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager HasBothSlots " & Exception_Information(Error));
            raise;
      END HasBothSlots;

      PROCEDURE SavePhySlot(PhySlot: SlotType; PhysAddr: LocoAddressType; Result: OUT Boolean) IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).PhysTrainAddr = PhysAddr THEN
               LookupTable(I).PhysSlotNum := PhySlot;
               LookupTable(I).HasPhySlot := True;
               Result := True;
               RETURN;
            END IF;
         END LOOP;
         Result := False;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager SavePhySlot " & Exception_Information(Error));
            raise;
      END SavePhySlot;

      PROCEDURE SaveVirtSlot(VirtSlot: SlotType; VirtAddr: LocoAddressType; Result: OUT Boolean) IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).VirtTrainAddr = VirtAddr THEN
               LookupTable(I).VirtSlotNum := VirtSlot;
               LookupTable(I).HasVirtSlot := True;
               Result := True;
               RETURN;
            END IF;
         END LOOP;
         Result := False;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager SaveVirSlot " & Exception_Information(Error));
            raise;
      END SaveVirtSlot;

      PROCEDURE SetTrainSensors(TrainId: TrainIdType; Sensors: SensorArrayType) IS
      BEGIN
         disposeSensorArray(LookupTable(TrainId).Sensors);
         LookupTable(TrainId).Sensors := NEW SensorArrayType(Sensors'RANGE);
         FOR I IN Sensors'RANGE LOOP
            LookupTable(TrainId).Sensors(I) := Sensors(I);
         END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager SetTrainSensors " & Exception_Information(Error));
            raise;
      END SetTrainSensors;

      PROCEDURE GetTrainSensors(TrainId: TrainIdType; Sensors: OUT SensorArrayAccess) IS
      BEGIN
         Sensors := NEW SensorArrayType(LookupTable(TrainId).Sensors'RANGE);
         FOR I IN Sensors'RANGE LOOP
            Sensors(I) := LookupTable(TrainId).Sensors(I);
         END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager GetTrainSensors " & Exception_Information(Error));
            raise;
      END GetTrainSensors;

   END SlotLookupTable;

   PROTECTED BODY TrainIdQueueList IS
   
      procedure listTrains is 
         TempEntry : TrainQueueEntry;
      begin
         Lists.moveFirst(TrainList);
         while Lists.hasCurrent(trainList) loop
            tempEntry := Lists.retrieveCurrent(trainList);
            put(natural'image(tempEntry.trainId));
            Lists.moveNext(trainList);
         end loop;
         new_line;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.listTrains " & Exception_Information(Error));
            raise;
      END listTrains;

      procedure removeTrain(trainId : trainIdType) is
         TempEntry : TrainQueueEntry;
         -- TrainIdNotFound : EXCEPTION;
      begin
         put("                   RemoveTrain before: "); listTrains;
         Lists.moveFirst(TrainList);
         while Lists.hasCurrent(trainList) loop
            tempEntry := Lists.retrieveCurrent(trainList);
            if tempEntry.trainId = trainId then
               -- makeEmpty(tempEntry.trainQueue);    -- memory leak
               Lists.removeCurrent(trainList);  
               put("                   RemoveTrain after: "); listTrains;
               return;
            end if;
            Lists.moveNext(trainList);
         end loop;
         raise trainIdNotFound;
      EXCEPTION
         -- WHEN TrainIdNotFound=>
            -- Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.removeTrain: Train id not found" & Exception_Information(TrainIdNotFound));
            -- raise;
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.removeTrain " & Exception_Information(Error));
            raise;
      END removeTrain;
         

      PROCEDURE AddTrain (TrainId : IN     TrainIdType) IS
         TempElement : TrainQueueEntry;
      BEGIN
         put("                   AddTrain before: "); listTrains;
         TempElement.TrainId := TrainId;
         TempElement.TrainQueue := NEW MessageQueueType;
         Lists.AddToEnd(TrainList, TempElement);
         put("                   AddTrain after: "); listTrains;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.AddTrain " & Exception_Information(Error));
            raise;
      END AddTrain;

      PROCEDURE GetQueue (
            TrainId    :        TrainIdType;
            TrainQueue :    OUT QueuePtr) IS
         TempEntry : TrainQueueEntry;
         -- TempList  : Lists.List;
         -- TrainIdNotFound : EXCEPTION;
      BEGIN
         Lists.moveFirst(TrainList);
         while Lists.hasCurrent(trainList) loop
            tempEntry := Lists.retrieveCurrent(trainList);
            if tempEntry.trainId = trainId then
               trainQueue := tempEntry.trainQueue;
               return;
            end if;
            Lists.moveNext(trainList);
         end loop;
         raise trainIdNotFound;
         -- WHILE NOT Lists.IsEmpty(TrainList) LOOP
            -- TempEntry := Lists.RetrieveFront(TrainList);
            -- Lists.RemoveFront(TrainList);
            -- IF TempEntry.TrainId = TrainId THEN
               -- TrainQueue := TempEntry.TrainQueue;
            -- END IF;
            -- Lists.AddToEnd(TempList, TempEntry);
         -- END LOOP;
         -- WHILE NOT Lists.IsEmpty(TempList) LOOP
            -- Lists.AddToEnd(TrainList, Lists.RetrieveFront(TempList));
            -- Lists.RemoveFront(TempList);
         -- END LOOP;
         -- IF TrainQueue = NULL THEN
            -- RAISE TrainIdNotFound;
         -- END IF;
      EXCEPTION
         -- WHEN TrainIdNotFound=>
            -- Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.GetQueue: Train id not found"  & Exception_Information(TrainIdNotFound));
            -- raise;
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.GetQueue " & Exception_Information(Error));
            raise;
      END GetQueue;

      PROCEDURE HasTrain (
            TrainId : TrainIdType;
            Result : OUT Boolean) IS
         TempEntry : TrainQueueEntry;
         -- TempList  : Lists.List;
      BEGIN
         Lists.moveFirst(TrainList);
         while Lists.hasCurrent(trainList) loop
            tempEntry := Lists.retrieveCurrent(trainList);
            if tempEntry.trainId = trainId then
               result := true;
               return;
            end if;
            Lists.moveNext(trainList);
         end loop;
         result := false;
         return;
         -- Result := False;
         -- WHILE NOT Lists.IsEmpty(TrainList) LOOP
            -- TempEntry := Lists.RetrieveFront(TrainList);
            -- Lists.RemoveFront(TrainList);
            -- IF TempEntry.TrainId = TrainId THEN
               -- Result := True;
            -- END IF;
            -- Lists.AddToEnd(TempList, TempEntry);
         -- END LOOP;
         -- WHILE NOT Lists.IsEmpty(TempList) LOOP
            -- Lists.AddToEnd(TrainList, Lists.RetrieveFront(TempList));
            -- Lists.RemoveFront(TempList);
         -- END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            Put_Line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.HasTrain " & Exception_Information(Error));
            raise;
      END HasTrain;

   END TrainIdQueueList;


END CommandQueueManager;

