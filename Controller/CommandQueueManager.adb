-- Message Queue Manager Package body
--4/8/2011
WITH Ada.Text_IO, Ada.Exceptions;
USE Ada.Text_IO, Ada.Exceptions;
WITH MessageTranslationLibrary; USE MessageTranslationLibrary;
with Tracer; use Tracer;
-- mo 12/16/11

PACKAGE BODY CommandQueueManager IS

   PROCEDURE Put (message : IN MessageType) IS
      TrainQueuePtr : QueuePtr;

   BEGIN
      --sort into correct list
      CASE message.ByteArray(1) IS
         when OPC_GPON | OPC_GPOFF | OPC_SW_STATE =>
            myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to out queue... ");
            CommandQueueManager.OutQueue.putMessage(message);
         when OPC_LOCO_SPD | OPC_LOCO_DIRF | OPC_LOCO_SND =>
            myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to train queue... ");
            TrainIdQueueList.GetQueue(Integer(message.ByteArray(2)), TrainQueuePtr);
            TrainQueuePtr.putMessage(Message);                    
         when OPC_SW_REQ | OPC_SW_REP | OPC_INPUT_REP =>
            myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to layout queue... ");
            LayoutQueue.putMessage(Message);
         when OPC_LOCO_ADR | OPC_SL_RD_DATA | OPC_LONG_ACK =>
            myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to SSI queue... ");
            SSIQueue.putMessage(Message);
         WHEN UZero =>         
            CASE message.ByteArray(2) IS
               when putPowerChangeComplete => 
                  myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to out queue... ");
                  CommandQueueManager.OutQueue.putMessage(message);
                when MsgTrainTaskQuit | MsgReinitializeTrain =>
                  myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to train queue... ");
                  TrainIdQueueList.GetQueue(Integer(message.ByteArray(3)), TrainQueuePtr);
                  TrainQueuePtr.putMessage(Message);                             
               WHEN DoLocoInit=>--send to SSI queue
                  myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to SSI queue... ");
                  SSIQueue.putMessage(Message);                
               when GetSwitchSuccessor | DoReadLayout | GetSwitchStates | DoMakeSectionUseable =>
                  myPutLine("  " & toEnglish(message) & "            ComQuMngr.Put to layout queue... ");
                  LayoutQueue.putMessage(Message);
               WHEN OTHERS =>
                  myPutLine("  Undefined Extended Message   ComQuMngr.Put: ");
            END CASE;
         WHEN OTHERS =>
            myPutLine("  Undefined Opcode     ComQuMngr.Put: ");
      END CASE;
      NULL;
   EXCEPTION
      WHEN error: OTHERS=>
         put_line("**************** EXCEPTION CommandQueueManager in Put: " & Exception_Information(Error));
         raise;
   END Put;

   PROCEDURE Get (message : OUT MessageType) IS
   BEGIN
      OutQueue.GetMessage(Message);
   EXCEPTION
      WHEN error: OTHERS=>
         put_line("**************** EXCEPTION CommandQueueManager: Get " & Exception_Information(Error));
         raise;
   END Get;


   PROTECTED BODY SocketListT IS

      PROCEDURE AddSocket (Socket : IN  socketType) IS
      BEGIN
         FOR I IN 1..(SocketListArray'Length-1) LOOP
            IF (Integer(SocketListArray(I)) = -1) THEN
               SocketListArray(I) := Socket;
               RETURN;
            END IF;
         END LOOP;
         RAISE SocketListFull;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SocketListT.AddSocket " & Exception_Information(Error));
            raise;
      END AddSocket;

      PROCEDURE AddRailroadSocket (Socket : IN socketType) IS
      BEGIN
         SocketListArray(0) := Socket;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SocketListT.AddRailRoadSocket " & Exception_Information(Error));
            raise;
      END AddRailroadSocket;


      PROCEDURE GetSocketListLength (Length : OUT Integer) IS
      BEGIN
         Length := SocketListArray'Length;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SocketListT.GetSocketListLength " & Exception_Information(Error));
            raise;
      END GetSocketListLength;

      PROCEDURE GetSocket (Index : IN  Integer; Socket : OUT socketType) IS
      BEGIN
         IF (Index >= 0) AND (Index < SocketListArray'Length) THEN
            Socket := SocketListArray(Index);
         END IF;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SocketListT.GetSocket " & Exception_Information(Error));
            raise;
      END GetSocket;

   END SocketListT;

   --------------------------------------------------------------------------
   
   PROTECTED BODY MessageQueueType IS                                      -- mo 12/22/11

      procedure putMessage(message : IN MessageType) IS
      BEGIN
         MessageQueue.Enqueue(Queue, Message);
         count := count + 1;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager putMessage " & Exception_Information(Error));
            raise;
      END putMessage;

      entry GetMessage(message : OUT MessageType) when count > 0 IS
      BEGIN
         MessageQueue.Dequeue(Queue, Message);
         count := count - 1;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager GetMessage " & Exception_Information(Error));
            raise;
      END GetMessage;

      function IsEmpty return boolean IS
      BEGIN
         return MessageQueue.IsEmpty(Queue);
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager IsEmpty " & Exception_Information(Error));
            raise;
      END IsEmpty;

   END MessageQueueType;
------------------------------------------------------------------------

   PROTECTED BODY SlotLookupTable IS


      procedure put is
         ent   : lookupEntry;
      begin
         myPutLine(" ");
         myPutLine("        (virtAddr virtSlot hasVirtSlot) (phyAddr phySlot hasPhySlot) inUse");
         for i in lookupTable'range loop
            ent := lookupTable(I);
            myPutLine("        (" & 
                     integer'image(ent.virtTrainAddr) &
                     integer'image(ent.virtSlotNum) & " " & 
                     boolean'image(ent.hasVirtSlot) & ") (" &
                     integer'image(ent.physTrainAddr) &
                     integer'image(ent.physSlotNum) & " " &
                     boolean'image(ent.hasPhySlot) & ") " &
                     boolean'image(ent.inuse));
         end loop;
         myPutLine(" ");
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager: Put " & Exception_Information(Error));
            raise;
      end put;

      procedure clearEntry(i : natural) is
         newEntry  : lookupEntry;
      begin
         if lookupTable(I).sensors /= null then
            disposeSensorArray(LookupTable(I).Sensors); 
         end if;
         lookupTable(i) := newEntry;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager: ClearEntry " & Exception_Information(Error));
            raise;
      end clearEntry;
      
      procedure clearTable is 
      begin
         for i in lookupTable'range loop
            clearEntry(i);
         end loop;
      EXCEPTION
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager: ClearTable " & Exception_Information(Error));
            raise;
      end clearTable;
      
      ----------------------------------------------------------
      -- return index of first empty entry in lookup table
      ---------------------------------------------------------
      PROCEDURE RequestTrainId (TrainId : OUT TrainIdType) IS
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
         WHEN error: OTHERS =>
            put_line("**************** EXCEPTION CommandQueueManager: RequestTrainId " & Exception_Information(Error));
            raise;
      END RequestTrainId;
    
      procedure removeEntryByPhysAddr(PhysAddr : LocoAddressType) is  
      begin
         FOR I IN LookupTable'RANGE LOOP
            IF lookupTable(I).physTrainAddr = PhysAddr THEN
               clearEntry(i);
               return;
            END IF;
         END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.removeEntryByPhysAddr " & Exception_Information(Error));
            raise;
      end removeEntryByPhysAddr;
      
      procedure removeEntryByEitherAddr(PhysAddr : LocoAddressType;
                                          VirtAddr : LocoAddressType) is  
      begin
         FOR I IN LookupTable'RANGE LOOP
            IF (physAddr /= 0 and lookupTable(I).physTrainAddr = PhysAddr) or
               (virtAddr /= 0 and lookupTable(i).virtTrainAddr = virtAddr) THEN
               clearEntry(i);
               return;
            END IF;
         END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.removeEntryByEitherAddr " & Exception_Information(Error));
            raise;
      end removeEntryByEitherAddr;
      
      procedure removeEntryByTrainId(trainId : trainIdType) is 
         I : trainIdType := trainId;
      begin
         clearEntry(i);
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.removeEntryByTrainId " & Exception_Information(Error));
            raise;
      end removeEntryByTrainId;
      
      PROCEDURE CreateEntry (
            VirtTrainAddr : IN     LocoAddressType;
            PhysTrainAddr : IN     LocoAddressType;
            TrainId       : IN     TrainIdType) IS
      BEGIN
         LookupTable(TrainId).PhysTrainAddr := PhysTrainAddr;
         LookupTable(TrainId).VirtTrainAddr := VirtTrainAddr;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager: SlotLookupTable.CreateEntry " & Exception_Information(Error));
            raise;
      END CreateEntry;

      FUNCTION TrainIdToVirtSlotNum(TrainId: TrainIdType) RETURN  SlotType IS
      BEGIN
         RETURN LookupTable(TrainId).VirtSlotNum;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager TrainIdToVirtSlotNum " & Exception_Information(Error));
            raise;
      END TrainIdToVirtSlotNum;

      FUNCTION TrainIdToPhysSlotNum(TrainId: TrainIdType) RETURN SlotType IS
      BEGIN
         RETURN LookupTable(TrainId).PhysSlotNum;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager TrainIdToPhysSlotNum " & Exception_Information(Error));
            raise;
      END TrainIdToPhysSlotNum;

      FUNCTION PhysSlotNumToTrainId(PhysSlotNum: SlotType) RETURN TrainIdType IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).PhysSlotNum = PhysSlotNum THEN
               RETURN I;
            END IF;
         END LOOP;
         return 0;  -- was RAISE EntryNotFound 
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager PhysSlotNumToTrainId " & Exception_Information(Error));
            raise;
      END PhysSlotNumToTrainId;

      procedure VirtSlotNumToTrainId(VirtSlotNum: SlotType; trainId : out trainIdType; found : out boolean) is
      BEGIN
         found := false;
         if virtSlotNum = 0 then   
            return;
         end if;
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).VirtSlotNum = VirtSlotNum THEN
               trainId := i;
               found := true;
               RETURN;
            END IF;
         END LOOP;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager VirtSlotNumToTrainId " & Exception_Information(Error));
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
            put_line("**************** EXCEPTION CommandQueueManager IsPhysAddrInTable " & Exception_Information(Error));
            raise;
      END IsPhysAddrInTable;

      function addressToVirtSlotNum(address : natural) return slotType is  -- mo 1/14/12   
      begin
         for i in lookupTable'range loop
            if lookupTable(i).hasVirtSlot and lookupTable(i).hasPhySlot and
            (lookupTable(i).virtTrainAddr = address or lookupTable(i).physSlotNum = address) then
               return lookupTable(i).virtSlotNum; 
            end if;
         end loop;
         return 0;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager addressToVirtSlotNum " & Exception_Information(Error));
            raise;
      end addressToVirtSlotNum;

      FUNCTION PhysAddrToTrainId(PhysAddr: LocoAddressType) RETURN TrainIdType IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).PhysTrainAddr = PhysAddr THEN
               RETURN I;
            END IF;
         END LOOP;
         return 0;  -- was RAISE EntryNotFound;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager PhysAddrToTrainId " & Exception_Information(Error));
            raise;
      END PhysAddrToTrainId;

      FUNCTION VirtAddrToTrainId(VirtAddr: LocoAddressType) RETURN
            TrainIdType IS
      BEGIN
         FOR I IN LookupTable'RANGE LOOP
            IF LookupTable(I).VirtTrainAddr = VirtAddr THEN
               RETURN I;
            END IF;
         END LOOP;
         RETURN 0;          
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager VirtAddrToTrainId " & Exception_Information(Error));
            raise;
      END VirtAddrToTrainId;

      FUNCTION TrainIdToVirtAddr(TrainId: TrainIdType) RETURN
            LocoAddressType IS
      BEGIN
         RETURN LookupTable(TrainId).VirtTrainAddr;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION CommandQueueManager TrainIdToVirAddr " & Exception_Information(Error));
            raise;
      END TrainIdToVirtAddr;

      FUNCTION TrainIdToPhysAddr(TrainId: TrainIdType) RETURN LocoAddressType IS
      BEGIN
         RETURN LookupTable(TrainId).PhysTrainAddr;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager TrainIdToPhysAddr " & Exception_Information(Error));
            raise;
      END TrainIdToPhysAddr;

      FUNCTION HasBothSlots(TrainId: TrainIdType) RETURN Boolean IS
      BEGIN
         RETURN LookupTable(TrainId).HasVirtSlot AND LookupTable(TrainId).HasPhySlot;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager HasBothSlots " & Exception_Information(Error));
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
            put_line("**************** EXCEPTION in CommandQueueManager SavePhySlot " & Exception_Information(Error));
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
            put_line("**************** EXCEPTION in CommandQueueManager SaveVirSlot " & Exception_Information(Error));
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
            put_line("**************** EXCEPTION in CommandQueueManager SetTrainSensors " & Exception_Information(Error));
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
            put_line("**************** EXCEPTION in CommandQueueManager GetTrainSensors " & Exception_Information(Error));
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
            myPutLine("              " & natural'image(tempEntry.trainId));
            Lists.moveNext(trainList);
         end loop;
         myPutLine(" ");
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.listTrains " & Exception_Information(Error));
            raise;
      END listTrains;

      procedure removeTrain(trainId : trainIdType) is
         TempEntry : TrainQueueEntry;
      begin
         myPutLine("                   RemoveTrain before: "); listTrains;
         Lists.moveFirst(TrainList);
         while Lists.hasCurrent(trainList) loop
            tempEntry := Lists.retrieveCurrent(trainList);
            if tempEntry.trainId = trainId then
               Lists.removeCurrent(trainList);  
               myPutLine("                   RemoveTrain after: "); listTrains;
               return;
            end if;
            Lists.moveNext(trainList);
         end loop;
         raise trainIdNotFound;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.removeTrain " & Exception_Information(Error));
            raise;
      END removeTrain;
         

      PROCEDURE AddTrain (TrainId : IN     TrainIdType) IS
         TempElement : TrainQueueEntry;
      BEGIN
         myPutLine("                   AddTrain before: "); listTrains;
         TempElement.TrainId := TrainId;
         TempElement.TrainQueue := NEW MessageQueueType;
         Lists.AddToEnd(TrainList, TempElement);
         myPutLine("                   AddTrain after: "); listTrains;
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.AddTrain " & Exception_Information(Error));
            raise;
      END AddTrain;

      PROCEDURE GetQueue (
            TrainId    :        TrainIdType;
            TrainQueue :    OUT QueuePtr) IS
         TempEntry : TrainQueueEntry;
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
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.GetQueue " & Exception_Information(Error));
            raise;
      END GetQueue;

      PROCEDURE HasTrain (
            TrainId : TrainIdType;
            Result : OUT Boolean) IS
         TempEntry : TrainQueueEntry;
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
      EXCEPTION
         WHEN error: OTHERS=>
            put_line("**************** EXCEPTION in CommandQueueManager.TrainIdQueueList.HasTrain " & Exception_Information(Error));
            raise;
      END HasTrain;

   END TrainIdQueueList;


END CommandQueueManager;

