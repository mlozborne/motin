WITH TrainPkg, CommandQueueManager, Globals, Interfaces, MessageTranslationLibrary;
WITH Ada.Text_IO, Ada.Exceptions;
USE TrainPkg, CommandQueueManager, Globals, Interfaces,
   MessageTranslationLibrary;
USE Ada.Text_IO, Ada.Exceptions;
with MessageTranslationTypes; use messageTranslationTypes;
with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;


PACKAGE BODY SSIPkg IS

   procedure CommandQueueManagerPut(msg : messageType) is
   begin
      put_line("    " & toEnglish(msg) & "       SSIPkg to ComQuMngr");      
      CommandQueueManager.put(msg);
   end CommandQueueManagerPut;

   PROCEDURE SendToOutQueue (
         message : MessageType) IS
   BEGIN
      put_line("    " & toEnglish(message) & "       SSIPkg to out queue");      
      -- IF message.ByteArray(2) = PutInitOutcome THEN
         -- Put_Line("    PutInitOutcome    SSIPkg to out queue");
      -- ELSIF message.ByteArray(1) = OPC_LOCO_ADR THEN
         -- Put_Line("    OPC_LOCO_ADR    SSIPkg to out queue ");
      -- ELSIF message.ByteArray(1) = OPC_MOVE_SLOTS THEN
         -- Put_Line("    OPC_MOVE_SLOTS    SSIPkg to out queue");
      -- elsif message.byteArray(1) = OPC_WR_SL_DATA then
         -- put_line("    OPC_WR_SL_DATA    SSIPkg to out queue");
      -- END IF;
      CommandQueueManager.OutQueue.putMessage(Message);
   EXCEPTION
      WHEN Error : OTHERS =>
         Put_Line("**************** EXCEPTION SSI pkg in SendToOutQueue: " & Exception_Information(Error));
         put_line("    opcode" & integer'image(integer(message.ByteArray(1))));
         raise;
   END SendToOutQueue;

   procedure unregisterOneTrainAndClearItsDCS200Slot(PhysAddr : LocoAddressType; 
                                                     LayoutPtr : LayoutManagerAccess) is
      trainId            : trainIdType;
      phySlot, virtSlot  : SlotType;
   begin
      -- Get train id
      TrainId := SlotLookupTable.PhysAddrToTrainId(PhysAddr);
      
      -- Tell the train task to quit
      CommandQueueManagerPut(makeTrainTaskQuitMsg(trainId));
      
      -- Free all sections occupied or reserved by train
      LayoutPtr.freeAllSectionsOccupiedOrReservedByTrain(trainId);
      
      -- Clear the DC200 slots used by train
      PhySlot := SlotLookupTable.TrainIdToPhysSlotNum(TrainId);      
      sendToOutQueue(makeWriteSlotDataToClearMsg(phySlot));
      VirtSlot := SlotLookupTable.TrainIdToVirtSlotNum(TrainId);
      sendToOutQueue(makeWriteSlotDataToClearMsg(virtSlot));
      
      -- Remove train from SlotLookupTable protected type in CommandQueueManager
      --                   TrainIdQueueList protected type in CommandQueueManager
      --                   TrainList in LayoutManager protected type in LayoutPkg      
      SlotLookupTable.removeEntryByPhysAddr(PhysAddr); 
      put_line("    In SSITask unregisterOneTrainAndClearItsDCS200Slot after removing entry by PhysAddr");
      slotLookupTable.put;
      TrainIdQueueList.removeTrain(trainId);   
      LayoutPtr.removeFromTrainList(trainId);
   EXCEPTION
      WHEN Error : OTHERS =>
         Put_Line("**************** EXCEPTION SSI pkg in unregisterOneTrainAndClearItsDCS200Slot: " & Exception_Information(Error));
         put_line("    PhysAddr" & integer'image(PhysAddr));
         raise;
   end unregisterOneTrainAndClearItsDCS200Slot;
   
   procedure unregisterAllTrainsAndClearDCS200SlotTable(LayoutPtr : LayoutManagerAccess) is
      physAddr  :  locoAddressType;
   begin
      for trainId in 1..kNumTrains loop
         physAddr := slotLookupTable.TrainIdToPhysAddr(trainId);
         if physAddr /= LocoAddressType'first then
           unregisterOneTrainAndClearItsDCS200Slot(PhysAddr, LayoutPtr);
         end if;
      end loop;
      put_line("    In SSITask unregisterAllTrainsAndClearDCS200SlotTable after removing entry by PhysAddr");
      slotLookupTable.put;
   EXCEPTION
      WHEN Error : OTHERS =>
         Put_Line("**************** EXCEPTION SSI pkg in unregisterAllTrainsAndClearDCS200SlotTable: " & Exception_Information(Error));
         raise;
   end unregisterAllTrainsAndClearDCS200SlotTable;
   
   procedure stopTrainAndDelay(PhysAddr : LocoAddressType) is 
      trainId : trainIdType;
   begin
      trainId := slotLookupTable.physAddrToTrainId(PhysAddr);
      SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedAbruptStop));
      put_line("       ********* Start train stop delay");
      delay kTrainStopDelay;
      put_line("       ********* End train stop delay");
   end stopTrainAndDelay;
   
   TASK BODY SSITaskType IS
      Cmd       : MessageType;
      LayoutPtr : LayoutManagerAccess;
      
      registeringPhysAddr     : natural := 0;       -- mo 1/12/12
      registeringVirtualAddr  : natural := 0;       -- mo 1/12/12      
   BEGIN
      ACCEPT SetLayout (L : IN  LayoutManagerAccess) DO
         LayoutPtr := L;
         TrainPkg.SetLayout(L);
      END;

      LOOP
         BEGIN
            --IF  SSIQueue.IsEmpty THEN               
            IF  false THEN    
               DELAY 0.01;      -- test 7                            
            else
               SSIQueue.GetMessage(Cmd);
               put_line("    " & toEnglish(cmd) & "....... received by SSITask");
               
               IF Cmd.ByteArray(1) = UZero AND Cmd.ByteArray(2) = DoLocoInit THEN   --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

                  DECLARE 
                     Result             : Boolean;
                     Count              : natural;
                     pCount             : positive;
                     Sensors            : SensorArrayAccess;   -- will be disposed at end of block
                     TrainId            : TrainIdType;
                     PhysAddr           : LocoAddressType;
                     PhySlot, VirtSlot  : SlotType;                        
                     VirtAddr           : LocoAddressType;
							sList					 : naturalListType;
                  BEGIN

                     SplitDoLocoInitMsg(Cmd, PhysAddr, sList);				
                     count := getCount(sList);
                     convertSensorListToArray(sList, sensors);
                     makeEmpty(sList);
                     -- Put_Line("    DoLocoInit with phyical address = " & Integer'Image(PhysAddr) & "    in SSITask");
                     if registeringPhysAddr /= 0 or registeringVirtualAddr /= 0 then       -- mo 1/12/12
                        -- Currently in the middle of initializing a train so ignore this DoLocoInit
                        put_line("    Try again when previous doLocoInit completes       in SSITask: ");
                        SendToOutQueue(makePutInitOutcomeMsg(PhysAddr, 123, 0, 123));  
                        
                     else  -- registeringPhysAddr = 0 and registeringVirtualAddr = 0
                        -- mo 2/8/12 vvvvvvvvvv
                        if PhysAddr = kClearAllSlotsAddress then 
                          Put_Line("    Unregistering all trains and clearing the DCS200 slot table          in SSITask: ");
                           -- stopAllTrains;
                           -- delay WaitTime;
                           unregisterAllTrainsAndClearDCS200SlotTable(LayoutPtr);
                        elsif count = 0 then
                           if  slotLookupTable.IsPhysAddrInTable(PhysAddr) then
                              --stopTrainAndDelay(PhysAddr);
                              unregisterOneTrainAndClearItsDCS200Slot(PhysAddr, LayoutPtr);
                           end if;
                        else
                        -- mo 2/8/12 ^^^^^^^^^^
                           pCount := positive(count);    
                           LayoutPtr.AreTrainSensorsLegal(pCount, Sensors.All, Result);
                           IF not Result THEN                 
                              Put_Line("    Illegal train sensors                       in SSITask: ");
                              SendToOutQueue(makePutInitOutcomeMsg(PhysAddr, 121, 0, 121));
                           ELSE
                              IF NOT SlotLookupTable.IsPhysAddrInTable(PhysAddr) THEN
                                 Put_Line("    Creating a new entry in the slotLookupTable                 in SSITask: ");
                                 SlotLookupTable.RequestTrainId(TrainId);
                                 VirtAddr := 10 + TrainId;
                                 SlotLookupTable.CreateEntry(VirtAddr, PhysAddr, TrainId);
                                 put_line("    Processing DoLocoInit after creating new entry      in SSITask ");
                                 slotLookupTable.put;
                                 SlotLookupTable.SetTrainSensors(TrainId, Sensors.All);
                                 
                                 registeringPhysAddr := PhysAddr;        -- mo 1/12/12
                                 registeringVirtualAddr := virtAddr;   -- mo 1/12/12
                                 
                                 --SpawnTrain(TrainId);                  -- state = halted / direction = forward / speed = 0
                                 SendToOutQueue(makeLocoAdrMsg(PhysAddr));
                                 SendToOutQueue(makeLocoAdrMsg(VirtAddr));
                              ELSE
                              
                                 Put_Line("    Reinitializing a train                        in SSITask: ");    
                                 --stopTrainAndDelay(PhysAddr);
                                 TrainId := SlotLookupTable.PhysAddrToTrainId(PhysAddr);                              
                                 PhySlot := SlotLookupTable.TrainIdToPhysSlotNum(TrainId);
                                 VirtSlot := SlotLookupTable.TrainIdToVirtSlotNum(TrainId);
                                 VirtAddr := SlotLookupTable.TrainIdToVirtAddr(TrainId);
                                 SlotLookupTable.SetTrainSensors(TrainId,Sensors.All);
                                 
                                 -- LayoutPtr.PositionTrain(TrainId, pCount, Sensors.All, Result);         -- mo 1/18/12             
                                 disposeSensorArray(sensors);
                                 SlotLookupTable.GetTrainSensors(TrainId, Sensors);                      
                                 LayoutPtr.repositionTrain(TrainId, pCount, Sensors.All, Result);          
                                 
                                 IF Result THEN
                                    SendToOutQueue(makePutInitOutcomeMsg(PhysAddr, PhySlot, VirtAddr, VirtSlot));
                                    convertSensorArrayToList(Sensors, sList);
                                    SendToOutQueue(makePutTrainPositionMsg(TrainId, sList));
                                    makeEmpty(sList);
                                    CommandQueueManagerPut(makeReinitializeTrainMsg(trainId));
                                 ELSE
                                    Put_Line("    Train's position conflicts with another train                  in SSITask: ");
                                    SendToOutQueue(makePutInitOutcomeMsg(PhysAddr, 124, 0, 124));
                                 END IF;      
                                                              
                               END IF;                          
                           end if;
                              
                         end if;
                     END IF;
                     disposeSensorArray(sensors);
                  END; -- declare
                  
               ELSIF Cmd.ByteArray(1) = OPC_SL_RD_DATA THEN  --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
               
                  DECLARE
                     Slot                  : SlotType;
                     -- Status                : Integer;
                     PhysAddr              : LocoAddressType;
                     VirtAddr              : LocoAddressType;
                     Result                : Boolean;
                     IsPhy                 : Boolean;
                     PhySlot,
                     VirtSlot              : SlotType;
                     TrainId               : TrainIdType;
                     Sensors               : SensorArrayAccess;
							sList		             : naturalListType;
							address	             : locoAddressType;
							addressIsAlreadyInUse : boolean;
                  BEGIN
                     -- SplitSlRdDataMsg(Cmd, Slot, Status, PhysAddr, VirtAddr, IsPhy);
							splitSlRdDataMsg(cmd, address, addressIsAlreadyInUse, slot);
							
							IF kFirstPhysAddress <= address AND address <= kLastPhysAddress THEN
								physAddr := address;
								IsPhy := True;
								VirtAddr := LocoAddressType'First;
							ELSE
								VirtAddr := Address;
								IsPhy := False;
								physAddr := LocoAddressType'First;
							END IF;
							
                     -- Put_Line("    OPC_SL_RD_DATA PhysAddr/virtAddr" & Integer'Image(PhysAddr) & "/" & natural'image(virtAddr) & "      in SSITask: ");
                     
                     -- IF IsPhy THEN                                                   -- mo 1/12/12
                        -- SlotLookupTable.SavePhySlot(Slot, PhysAddr, Result);
                     -- ELSE
                        -- SlotLookupTable.SaveVirtSlot(Slot, VirtAddr, Result);
                     -- END IF;
                     
                     IF IsPhy and registeringPhysAddr = PhysAddr THEN                   -- mo 1/12/12
                        SlotLookupTable.SavePhySlot(Slot, PhysAddr, Result);
                        put_line("    OPC_SL_RD_DATA processing after saving physical slot number       in SSITask"); 
                        slotLookupTable.put;
                        registeringPhysAddr := 0;
                     ELSif not isPhy and registeringVirtualAddr = virtAddr then
                        SlotLookupTable.SaveVirtSlot(Slot, VirtAddr, Result);
                        put_line("    OPC_SL_RD_DATA processing after saving virtual slot number       in SSITask"); 
                        slotLookupTable.put;
                        registeringVirtualAddr := 0;
                     else
                        result := false;
                     END IF;
                     
                     IF Result THEN
                        SendToOutQueue(makeMoveSlotsMsg(Slot, Slot));
                        IF IsPhy THEN
                           TrainId := SlotLookupTable.PhysAddrToTrainId(PhysAddr);
                           VirtAddr := SlotLookupTable.TrainIdToVirtAddr(TrainId);
                        ELSE
                           TrainId := SlotLookupTable.VirtAddrToTrainId(VirtAddr);
                           PhysAddr := SlotLookupTable.TrainIdToPhysAddr(TrainId);
                        END IF;
                        IF not SlotLookupTable.HasBothSlots(TrainId) THEN
                           null;   -- wait until have both slots
                        else
                           SpawnTrain(TrainId);   
                           PhySlot := SlotLookupTable.TrainIdToPhysSlotNum(TrainId);
                           VirtSlot := SlotLookupTable.TrainIdToVirtSlotNum(TrainId);
                           
                           disposeSensorArray(sensors);
                           SlotLookupTable.GetTrainSensors(TrainId, Sensors);                      
                           LayoutPtr.PositionTrain(TrainId, Sensors'Length, Sensors.All, Result);  
                           
                           IF Result THEN
                              SendToOutQueue(makePutInitOutcomeMsg(PhysAddr, PhySlot, VirtAddr, VirtSlot));
                              CommandQueueManagerPut(makeReinitializeTrainMsg(trainId));
                              -- SendToOutQueue(makeLocoSpdMsg(TrainId, 0));
                              -- SendToOutQueue(makeLocoDirfMsg(TrainId, Forward, Off, Off, Off));
                              -- SendToOutQueue(makeLocoSndMsg(TrainId, Off));
                              -- SendToOutQueue(makePutTrainInformationMsg(TrainId, 0, Forward, Off, Off, Off, Off));
										
										convertSensorArrayToList(Sensors, sList);										
                              SendToOutQueue(makePutTrainPositionMsg(TrainId, sList));
										makeEmpty(sList);
                           ELSE
                              Put_Line("    Train's position conflicts with another train           in SSITask: ");
                              SendToOutQueue(makePutInitOutcomeMsg(PhysAddr, 124, 0, 124));
                           END IF;
                        END IF;
                     END IF;
                     disposeSensorArray(sensors);
                  END; -- declare
                  
               ELSIF Cmd.ByteArray(1) = OPC_LOCO_ADR THEN --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  -- mo 1/12/12
               
                  -- Only messages coming from a throttle attached to the controller reach here.                   
                  -- if the address is neither a physical nor virtual address in a completed entry of 
                  -- the slot lookup table then
                  --   send OPC_LONG_ACK
                  -- else
                  --   send OPC_SL_RD_DATA 
                  -- end if
                  declare
                     address         : natural;
                     trainId         : natural;
                  begin
                     SplitLocoAdrMsg(cmd, Address);  
                     -- Put_Line("    OPC_LOCO_ADR address = " & integer'image(address) & "    in SSITask");
                     trainId := slotLookupTable.addressToTrainId(address);
                     if trainId = 0 then
                        Put_Line("    Table entry for this address is not complete             in SSITask: ");
                        sendToOutQueue(makeLongAckMsg(opc_loco_adr));
                     else
                        sendToOutQueue(makeSlRdDataMsg(trainId, address));
                     end if;
                  end;
                  
               elsif cmd.byteArray(1) = opc_long_ack then --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  -- mo 1/15/12
               
                  -- Put_Line("    OPC_LONG_ACK        in SSITask: ");
                  if registeringPhysAddr = 0 and registeringVirtualAddr = 0 then
                     null;
                  else
                     Put_Line("    Insufficient slots  for physAddr/virtAddr " & 
                              natural'image(registeringPhysAddr) &  natural'image(registeringVirtualAddr) & "  in SSITask: ");
                     declare
                        trainId              : trainIdType;
                        phySlot, virtSlot    : slotType;
                     begin
                        -- Clear the entry in the SlotLookupTable and in the DCS200 slot table
                        if registeringPhysAddr = 0 then
                           trainId :=  SlotLookupTable.VirtAddrToTrainId(registeringVirtualAddr);
                        else
                           TrainId := SlotLookupTable.PhysAddrToTrainId(registeringPhysAddr);
                        end if;
                        PhySlot := SlotLookupTable.TrainIdToPhysSlotNum(TrainId);      
                        if phySlot /= 0 then
                           sendToOutQueue(makeWriteSlotDataToClearMsg(phySlot));
                        end if;
                        VirtSlot := SlotLookupTable.TrainIdToVirtSlotNum(TrainId);
                        if virtSlot /= 0 then
                           sendToOutQueue(makeWriteSlotDataToClearMsg(virtSlot));
                        end if;
                        SlotLookupTable.removeEntryByTrainId(trainId); 
                        slotLookupTable.put;
         
                        -- Tell the admin throttle
                        SendToOutQueue(makePutInitOutcomeMsg(registeringPhysAddr, 122, 0, 122));
                        
                        -- Get ready for the next doLocoInit
                        registeringPhysAddr := 0;
                        registeringVirtualAddr := 0;
                     end;
                     
                  end if;
                     
               elsif cmd.byteArray(1) = opc_move_slots then --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  -- mo 1/15/12

                  null;
                  -- Put_Line("    OPC_MOVE_SLOTS                  in SSITask: ");
                  
               END IF;
            END IF;                                      
         EXCEPTION
            WHEN Error : OTHERS =>
               Put_Line("**************** EXCEPTION: SSIPkg in SSITask " & Exception_Information(Error));
         END;
      END LOOP;
   END SSITaskType;
END SSIPkg;