WITH MessageTranslationLibrary;
WITH Input_Sources.File, XMLParser, Ada.Exceptions, CommandQueueManager, Interfaces;
USE Input_Sources.File, XMLParser, Ada.Exceptions, CommandQueueManager, Interfaces;
USE MessageTranslationLibrary;
with Tracer; use Tracer;

PACKAGE BODY LayoutPkg IS

------------------------ 1 ----------------------------------
------------------------ 1 ----------------------------------
------------------------ 1 ----------------------------------
------- Begin declarations of types used by LayoutManagerPkg 

   -- Put the Message to the OutQueue
   -- and output the message to the screen
   PROCEDURE SendToOutQueue (
         Cmd : MessageType) IS
   BEGIN
      myPutLine("      " & toEnglish(cmd) & "       LayoutPkg to out queue");
      CommandQueueManager.OutQueue.putMessage(Cmd);      
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("**************** EXCEPTION Layout pkg in SendToOutQueue: " & Exception_Information(Error));
         RAISE;
   END SendToOutQueue;
   
---- End declarations of types used by LayoutManager 
------------------------ 1 ----------------------------------
------------------------ 1 ----------------------------------
------------------------ 1 ----------------------------------


------------------------ 2 ---------------------------------------
------------------------ 2 ---------------------------------------
------------------------ 2 ---------------------------------------
-------------------- Begin LayoutManager -------------------------

   PROTECTED BODY LayoutManager IS
   
   ----------------------- 2a ----------------------------
   ----------------------- 2a ----------------------------
   ----------------------- 2a ----------------------------
   -- Begin functions to manipulate data structures    

      PROCEDURE AreTrainSensorsLegal (
            Count   :        positive;
            Sensors :        SensorArrayType;
            Legal   :    OUT Boolean) IS
         FirstSensor    : Positive;
         SecondSensor   : Positive;
         ThisSectionPtr : SectionNodePtr;
         PrevSectionPtr : SectionNodePtr;
      BEGIN
         IF Sensors'Length = Count AND Count > 1 THEN
            FirstSensor := Sensors(1);
            FOR I IN Sensors'First+1..Sensors'Last LOOP
               SecondSensor := Sensors(I);
               FindSection(FirstSensor, SecondSensor, ThisSectionPtr);
               IF ThisSectionPtr = NULL THEN
                  Legal := False;
                  RETURN;
               END IF;
               IF PrevSectionPtr /= NULL THEN
                  IF PrevSectionPtr.SensorList.head.id /= SecondSensor AND
                        NOT IsIn(PrevSectionPtr.NextSectionList, ThisSectionPtr.Id) AND
                        PrevSectionPtr.SensorList.tail.id /= SecondSensor AND
                        NOT IsIn(PrevSectionPtr.PrevSectionList, ThisSectionPtr.Id) THEN
                     Legal := False;
                     RETURN;
                  END IF;
               END IF;
               PrevSectionPtr := ThisSectionPtr;
               FirstSensor := Sensors(I);
            END LOOP;
            Legal := True;
         ELSE
            Legal := False;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AreTrainSensorsLegal: " & Exception_Information(Error));
            put_line("    count" & integer'image(count));
            raise;
      END AreTrainSensorsLegal;

      -- Change a train's direction
      PROCEDURE ChangeDirectionOf (TrainId : TrainIdType) IS
         TrainPtr      : trainNodePtr   := TrainList;
         ThisSensorPtr : SensorNodePtr;
         PrevSensorPtr : SensorNodePtr := null; 
         NextSensorPtr : SensorNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               ThisSensorPtr := TrainPtr.SensorList.Head;
               TrainPtr.SensorList.Head := TrainPtr.SensorList.Tail;
               TrainPtr.SensorList.Tail := ThisSensorPtr;
               WHILE ThisSensorPtr /= NULL LOOP
                  NextSensorPtr := ThisSensorPtr.Next;
                  ThisSensorPtr.Next := PrevSensorPtr;
                  PrevSensorPtr := ThisSensorPtr;
                  ThisSensorPtr := NextSensorPtr;
               END LOOP;
               ReleaseReservation(TrainId);
               declare   
                  sensorCount : natural := TrainPtr.SensorCount;                                                                    
                  mySensors   : sensorArrayAccess := new sensorArrayType(1..sensorCount);
                  I           : positive := 1;
                  sensorPtr   : sensorNodePtr;
               begin
                  SensorPtr := TrainPtr.SensorList.Head;
                  WHILE SensorPtr /= NULL LOOP
                     mySensors(I) := sensorPtr.id;
                     I := I + 1;
                     SensorPtr := SensorPtr.Next;
                  END LOOP;
              
                  PutTrainPositionMsg(TrainId);
              
               end;
               RETURN;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in ChangeDirectionOf: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END ChangeDirectionOf;

      procedure freeAllSectionsOccupiedOrReservedByTrain(trainId : TrainIdType) is
         SectionPtr  : SectionNodePtr;
      begin        
         sectionPtr := sectionList.head;              
         while sectionPtr /= null loop
            if sectionPtr.trainId = trainId and (sectionPtr.state = occupied or sectionPtr.state = reserved) then
               sectionPtr.state := Free;
               sectionPtr.trainId := 0;
               ReleaseBlockings(SectionPtr.BlockingList);
               SendToOutQueue(makePutSectionStateMsg(SectionPtr.Id, Free));
            end if;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in freeAllSectionsOccupiedOrReservedByTrain: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      end freeAllSectionsOccupiedOrReservedByTrain;
      
      PROCEDURE GetSwitchStates IS
         SwitchPtr : SwitchNodePtr := SwitchList.Head;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            SendToOutQueue(makePutSwitchStateMsg(switchPtr.id, switchPtr.state));
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetSwitchStates: " & Exception_Information(Error));
            raise;
      END GetSwitchStates;
            
      PROCEDURE IdentifyTrainV3 (sx : Positive) IS
         expectationError    : boolean;
         SensorPtr          : SensorNodePtr;
         s1                  : sensorNodePtr;
         leftSectionPtr     : sectionNodePtr;
         rightSectionPtr    : sectionNodePtr;
         -- nextFreeSection    : sectionNodePtr;
         section            : sectionNodePtr;
         sectionNode        : sectionNodePtr;
         oldSensorState     : sensorStateType;
         newSensorState     : sensorStateType;
         trainId             : trainIdType;
         SensorPtrs         : accessArraySensorNodePtrType;
         last               : positive;
         idSensorCase       : positive;
      begin
         -- Determine if sensor is in the layout specification even if it exists physical.
         FindSensor(SensorList, sx, SensorPtr);
         IF SensorPtr = NULL THEN
            myPutLine("      -------------IdentifyTrainV3: ERROR(maybe) sensor not recognized " & integer'image(sx) ); 
            return;
         end if;
         
         -- Remember the sensor's state:  oldSensorState
         -- Flip the sensor:  newSensorState
         -- Display the old sensor state
         oldSensorState := sensorPtr.state;
         flipSensor(sensorPtr);
         newSensorState := sensorPtr.state;
         if oldSensorState = open then
            myPutLine("      -------------IdentifyTrainV3: sensor " & integer'image(sx) & " open-->closed");   
         else
            myPutLine("      -------------IdentifyTrainV3: sensor " & integer'image(sx) & " closed-->open");   
         end if;

         -- Inform Othrottles that sensor has fired and its new state                 
         SendToOutQueue(makePutSensorStateMsg(sx, newSensorState));

         -- Identify the sensor
         -- If trainId is 0 then expectationError should be 6
         identifySensor(sx, idSensorCase, expectationError, trainId, leftSectionPtr, rightSectionPtr);
         
         if idSensorCase = 1 then
         
            -- Case 1: sx not in controller’s sensor list
            -- We have already eliminated this case above by calling FindSensor
            null;
         
         elsif idSensorCase = 2 then   
         
            -- Case 2: sx = sn (even if sx = t1 or sx=tf for another train
            if expectationError then
               errorStopTrainsAtBadSensor(idSensorCase, sx, leftSectionPtr, rightSectionPtr);
            else
               if oldSensorState = closed then
                  myPutLine("      -------------IdentifyTrainV3: C2 NORMAL back of train leaving closed sensor sn"); 
               else
                  myPutLine("      -------------IdentifyTrainV3: C2 SENSOR ERROR back of train leaving open sensor. Stop train.");
                  sendToTrainQueue(makeSensorErrorMsg(sx), trainId);   
               end if;
            end if;
            
         elsif idSensorCase = 3 then
         
            -- Case 3: sx=si in <s2,...,sn-1>, where n  > 2
            if expectationError then
               errorStopTrainsAtBadSensor(idSensorCase, sx, leftSectionPtr, rightSectionPtr);
            else
               SensorPtrs := GetSensorPtrs(TrainId); -- This array contains pointers to s1,s2,...,sn
               last := sensorPtrs'last;
               
               if oldSensorState = closed then
                  myPutLine("      -------------IdentifyTrainV3: C3 FIXING sensor unexpectedly closed. Flip and continue"); 
                  flipSensor(sensorPtr);
                  SendToOutQueue(makePutSensorStateMsg(sx, closed));
               end if;
               -- NORMAL from here if sn-1 fired else fixing by removing extra sensorStateType
               for i in reverse 2..last loop
                  exit when sx = sensorPtrs(i).id;
                  myPutLine("      -------------IdentifyTrainV3: C3 removing sensor"  
                            & integer'image(sensorPtrs(i).id) & " from back of train"); 
                  sensorPtrs(i).state := open;   -- Open si for safety
                  SendToOutQueue(makePutSensorStateMsg(sensorPtrs(i).id, open));               
                  getSection(sectionNode, sensorPtrs(i-1).id, sensorPtrs(i).id);
                  section := sectionNode;
                  section.state := free;
                  SendToOutQueue(makePutSectionStateMsg(section.Id, Free));
                  ReleaseBlockings(section.BlockingList);
                  section.trainId := 0;
                  RemoveLastSensor(TrainId);
                  SendToTrainQueue(makeBackSensorFiredMsg(TrainId), TrainId);
               end loop;   
               putTrainPositionMsg(TrainId);
               SendToAllTrainQueues(makeTryToMoveAgainMsg);               
               disposeArrayOfsensorNodePtr(sensorPtrs);            
            end if;
            
         elsif idSensorCase = 4 then
         
            -- Case 4: sx = s1 (and by Case 2 sx/=tn for all other trains)
            if expectationError then
               errorStopTrainsAtBadSensor(idSensorCase, sx, leftSectionPtr, rightSectionPtr);
            else
               if oldSensorState = open then
                  myPutLine("      -------------IdentifyTrainV3: C4 NORMAL front of train approaching sensor s1, ignore.");      
               else 
                  myPutLine("      -------------IdentifyTrainV3: C4 NORMAL front of train leaving sensor s1.");   
                  leftSectionPtr.state := occupied;
                  sendToOutQueue(makePutSectionStateMsg(leftSectionPtr.Id, Occupied));
                  IF leftSectionPtr.SensorList.head.id = sx THEN
                     AddNewSensorToFront(leftSectionPtr.TrainId, leftSectionPtr.SensorList.tail);   
                  ELSE
                     AddNewSensorToFront(leftSectionPtr.TrainId, leftSectionPtr.SensorList.head);
                  END IF;
                  if countSensors(trainId) > kMaxTrainLength + 1 then
                     myPutLine("      -------------IdentifyTrainV3: C4 ERROR train " & integer'image(trainId) & 
                               " too long. Error stop train");   
                     sendToTrainQueue(makeSensorErrorMsg(sx), trainId);                     
                  else   
                     SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);  -- Tell train front sensor has fired
                  end if;
                  PutTrainPositionMsg(TrainId);                                    -- Put train position
               end if;
            end if;
            
         elsif idSensorCase = 5 then
         
         -- Case 5: sx = sf (and by the previous cases, sx belongs to no other train)
            if expectationError then
               errorStopTrainsAtBadSensor(idSensorCase, sx, leftSectionPtr, rightSectionPtr);
            else
               -- In this section of code we now use s1.id instead of sx = sf
               if oldSensorState = closed then
                  myPutLine("      -------------IdentifyTrainV3: C5 ERROR sf closed-->open should have handled this when open-->closed. Error stop train");
               else
                  myPutLine("      -------------IdentifyTrainV3: C5 Just reached sf, we want to open s1 and handle s1 closed-->open");
                  myPutLine("      -------------IdentifyTrainV3: C5 NORMAL front of train leaving sensor s1.");
                  rightSectionPtr.state := occupied;
                  SendToOutQueue(makePutSectionStateMsg(rightSectionPtr.Id, Occupied));
                  s1 := getFrontSensorPtr(trainId); 
                  s1.state := open;
                  SendToOutQueue(makePutSensorStateMsg(s1.id, open));
                  IF rightSectionPtr.SensorList.head.id = s1.id THEN                
                     AddNewSensorToFront(rightSectionPtr.TrainId, rightSectionPtr.SensorList.tail);   
                  ELSE
                     AddNewSensorToFront(rightSectionPtr.TrainId, rightSectionPtr.SensorList.head);
                  END IF;
                  
                  if countSensors(trainId) > kMaxTrainLength + 1 then
                     myPutLine("      -------------IdentifyTrainV3: C5 ERROR train " & integer'image(trainId) & 
                               " too long. Error stop train");   
                     sendToTrainQueue(makeSensorErrorMsg(s1.id), trainId);                     
                  else   
                     SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);  -- Tell train front sensor has fired
                  end if;
                  PutTrainPositionMsg(TrainId);                                    -- Put train position
               end if;
            end if;
         
            -- if expectationError then
               -- errorStopTrainsAtBadSensor(idSensorCase, sx, leftSectionPtr, rightSectionPtr);
            -- else
               -- s1 := getFrontSensorPtr(trainId);
               -- sf := getSensorAtFrontOfReservedSectionPtr(rightSectionPtr.all);
               -- if oldSensorState = open then
                  -- myPutLine("      -------------IdentifyTrainV3: C5 IGNORE front of train approaching sf. Fix when leaving");
               -- else
                  -- myPutLine("      -------------IdentifyTrainV3: C5 FIXING front of train leaving sf.");   
                  -- rightSectionPtr.state := occupied;
                  -- SendToOutQueue(makePutSectionStateMsg(rightSectionPtr.Id, Occupied));
                  -- getFreeSection(rightSectionPtr.nextSectionList, nextFreeSection);
                  -- if nextFreeSection = null then 
                     -- getFreeSection(rightSectionPtr.prevSectionList, nextFreeSection);
                  -- end if;
                  -- if nextFreeSection = null then
                     -- myPutLine("      -------------IdentifyTrainV3: C5 ERROR couldn't fix, next section blocked. Error stop train");   
                     -- sendToTrainQueue(makeSensorErrorMsg(sx), trainId);      
                  -- else 
                     -- nextFreeSection.state := occupied;
                     -- BlockSections(nextFreeSection.BlockingList);
                     -- nextFreeSection.trainId := trainId;
                     -- SendToOutQueue(makePutSectionStateMsg(nextFreeSection.Id, Occupied));
                     -- s1.state := open;   -- Set s1 open for safety
                     -- SendToOutQueue(makePutSensorStateMsg(s1.id, open));
                     -- if rightSectionPtr.sensorList.head.id = sf.id then    -- Add sf to front of train
                        -- addNewSensorToFront(trainId, rightSectionPtr.sensorList.head);  
                     -- else
                        -- addNewSensorToFront(trainId, rightSectionPtr.sensorList.tail);
                     -- end if;
                     -- if nextFreeSection.sensorList.head.id = sf.id then   -- Add sf+1 to front of train
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.tail);
                     -- else
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.head);
                     -- end if;
                     -- if countSensors(trainId) > kMaxTrainLength + 1 then
                        -- myPutLine("      -------------IdentifyTrainV3: C5 ERROR train " & integer'image(trainId) & 
                                  -- " too long. Error stop train");   
                        -- sendToTrainQueue(makeSensorErrorMsg(sx), trainId);                     
                     -- else   
                        -- SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);  -- Tell train front sensor has fired
                     -- end if;
                     -- PutTrainPositionMsg(TrainId);                                    -- Put train position
                  -- end if;
               -- end if;
            -- end if;
            
         else
         
            -- Case 6: other, such as the sensor is not associated with any train
            myPutLine("      -------------IdentifyTrainV3: C6 MYSTERY SENSOR FIRING not close to any trains: error stop all" );
            SendToAllTrainQueues(makeSensorErrorMsg(sx));
         
         end if;
                        
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IdentifyTrainV3: " & Exception_Information(Error));
            raise;
      end IdentifyTrainV3;
      
      -- PROCEDURE IdentifyTrainV2 (SensorID : Positive) IS
         -- sx                   : SensorNodePtr;   -- ptr to sensor that fired
         -- sf                   : sensorNodePtr;    -- next sensor in front of train
         -- t1                   : sectionNodePtr;   -- first section containing sx
         -- t2                   : sectionNodePtr;   -- second section containing sx
         -- reservedSection      : sectionNodePtr;
         -- occupiedSection      : sectionNodePtr;
         -- lastSection          : sectionNodePtr;
         -- BackId               : Positive;
         -- oldSensorState       : sensorStateType;
         -- newSensorState       : sensorStateType;
         -- trainId              : trainIdType;
         -- sensorsPtr           : sensorArrayAccess; -- <s1..sn>
      -- BEGIN
          -- myPutLine("      xxxxxxxxxxxxx: In IdentifyTrainV2, sensor = " & integer'image(sensorId)); 
          
         -- -- Get a pointer to the sensor object.
         -- -- If not found then ignore this sensor and return
         -- FindSensor(SensorList, SensorID, sx);         
         -- IF sx = NULL THEN
            -- myPutLine("      xxxxxxxxxxxxx: MAYBE ERROR sensor not recognized " & integer'image(sensorId) ); 
            -- return;
         -- end if;
         
         -- -- Keep track of the old and new state of the sensor
         -- oldSensorState := sx.state;
         -- flipSensor(sx);
         -- newSensorState := sx.state;
         -- myPutLine("      xxxxxxxxxxxxx: Sensor state = " & sensorStateType'image(oldSensorState)); 
         
         -- -- Inform Othrottles that sensor has fired and its new state                 -- mo 1/30/12
         -- SendToOutQueue(makePutSensorStateMsg(SensorId, newSensorState));
         
         -- -- Find the two sections that contain the sensor
         -- getUnbockedUsableSectionsContainingSensor(sx.id, t1, t2);
         -- if t1 /= null then
            -- myPutLine("      xxxxxxxxxxxxx: 1st section id, state, trainId = " 
                      -- & integer'image(t1.id) & " " 
                      -- & sectionStateType'image(t1.state) & " " 
                      -- & integer'image(t1.trainId)); 
         -- end if;
         -- if t2 /= null then
            -- myPutLine("      xxxxxxxxxxxxx: 2st section id, state, trainId = " 
                      -- & integer'image(t2.id) & " " 
                      -- & sectionStateType'image(t2.state) & " " 
                      -- & integer'image(t2.trainId)); 
         -- end if;
         
         -- -- If either section is undefined, then put all trains in an error state and return
         -- if t1 = null or t2 = null then
            -- myPutLine("      xxxxxxxxxxxxx: XML, LOGIC, OR SENSOR FIRING ERROR can't find two unblocked sections for sensor"); 
            -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            -- return;
         -- end if;
                     
         -- -- If the sections involve different trains 
         -- -- and the sensor is not a back sensor of either 
         -- -- then put both trains in an error state and return
         -- if t1.trainId /= 0 
         -- and then t2.trainId /= 0 
         -- and then t1.TrainId /= t2.TrainId 
         -- and then (sensorId /= getBackSensor(t1.trainId) and sensorId /= getBackSensor(t2.trainId))
         -- then
            -- myPutLine("      xxxxxxxxxxxxx: LOGIC ERROR sensor involves two trains:" & integer'image(sensorId) ); 
            -- sendToTrainQueue(makeSensorErrorMsg(SensorId), t1.trainId);
            -- sendToTrainQueue(makeSensorErrorMsg(SensorId), t2.trainId);
            -- return;
         -- end if;
         
         -- -- If the sections involve no trains then logic error, put all trains in error state, and return
         -- if t1.trainId = 0 and t2.trainId = 0 then
            -- myPutLine("      xxxxxxxxxxxxx: LOGIC ERROR: no train ids found"); 
            -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            -- return;
         -- end if;
                     
         -- -- If neither section is occupied then put all trains in an error state and return
         -- if t1.state /= occupied and t2.state /= occupied then
            -- myPutLine("      xxxxxxxxxxxxx: RANDOM SENSOR FIRING ERROR no trains nearby " 
                      -- & integer'image(sensorId)); 
            -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            -- return;
         -- end if;
               
         -- -- If sections are occupied by different trains then go with the train whose back sensor was fired
         -- -- Else go with the train in the occupied section
         -- if        (t1.state = occupied and t2.state = occupied) 
         -- and then  (t1.trainId /= t2.trainId)
         -- then
            -- if sensorId = getBackSensor(t1.trainId) then
               -- trainId := t1.trainId;
            -- elsif sensorId = getBackSensor(t2.trainId) then
               -- trainId := t2.trainId;
            -- else
               -- myPutLine("      xxxxxxxxxxxxx: SENSOR FIRING ERROR: sensor doesn't match back of either train" 
                         -- & integer'image(sensorId)); 
               -- sendToTrainQueue(makeSensorErrorMsg(SensorId), t1.trainId);
               -- sendToTrainQueue(makeSensorErrorMsg(SensorId), t2.trainId);
               -- return;
            -- end if;
         -- elsif t1.state = occupied then
            -- trainId := t1.trainId;
         -- else
            -- trainId := t2.trainId;
         -- end if;

               
         
         -- sensorsPtr := GetSensors(TrainId);
         -- myPutLine("      xxxxxxxxxxxxx: Id of train being processed = " & integer'image(trainId) & " *******************************");         
         -- -- Identify the fired sensor relative to the train's sensors
         -- if sensorId = sensorsPtr(1) then       
         
            -- -- First sensor fired   (sx = s1)
            
            -- -- Ignore if open --> closed
            -- if oldSensorState = open then
               -- myPutLine("      xxxxxxxxxxxxx: front of train approaching sensor, ignored " & integer'image(sensorId)); 
               -- disposeSensorArray(sensorsPtr);
               -- return;
            -- end if;
            
            -- -- Closed --> open
            -- myPutLine("      xxxxxxxxxxxxx: front of train leaving sensor, processing " & integer'image(sensorId)); 
            
            -- -- Determine which section is reserved, which occupied
            -- if t1.state = reserved and t2.state = occupied then
               -- reservedSection := t1;
               -- occupiedSection := t2;
            -- elsif t1.state = occupied and t2.state = reserved then
               -- reservedSection := t2;
               -- occupiedSection := t1;
            -- else
               -- myPutLine("      xxxxxxxxxxxxx: LOGIC ERROR should have had one section occupied, one reserved " 
                         -- & integer'image(sensorId) ); 
               -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               -- disposeSensorArray(sensorsPtr);
               -- return;
            -- end if;
            
            -- -- Determine the next sensor in front of train
            -- if reservedSection.sensorList.head.id = sensorId then
               -- sf := reservedSection.sensorList.tail;
            -- else   
               -- sf := reservedSection.sensorList.head;
            -- end if;
            
            -- -- Change reserved section to occupied.
            -- -- Send section state message.
            -- -- Add new sensor to front of train.
            -- -- Tell train front sensor fired.
            -- reservedSection.State := Occupied;
            -- SendToOutQueue(makePutSectionStateMsg(reservedSection.Id, Occupied));            
            -- AddNewSensorToFront(TrainId, sf);
            -- SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);

            -- PutTrainPositionMsg(TrainId);
            
            -- disposeSensorArray(sensorsPtr);            
            -- return;
            
         -- elsif sensorId = sensorsPtr(sensorsPtr'last - 1) then
         
            -- -- Second to last sensor fired  (sx = sn-1)
            
            -- -- Logic error if closed --> open
            -- if oldSensorState = closed then
               -- myPutLine("      xxxxxxxxxxxxx: LOGIC ERROR sensor sn-1 closed --> open  " & integer'image(sensorId) ); 
               -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               -- disposeSensorArray(sensorsPtr);
               -- return;
            -- end if;
            
            -- -- Open --> closed
            -- myPutLine("      xxxxxxxxxxxxx: back of train approaching sensor, processing " & integer'image(sensorId) ); 
            
            -- -- Error return if both sections not occupied
            -- if t1.state /= occupied and t2.state /= occupied then
               -- myPutLine("      xxxxxxxxxxxxx: LOBIC ERROR should have both sections occupied " & integer'image(sensorId) ); 
               -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               -- disposeSensorArray(sensorsPtr);
               -- return;
            -- end if;

            -- -- Determine which section is last
            -- backId := sensorsPtr(sensorsPtr'last);
            -- IF t1.sensorList.head.id = backId OR t1.SensorList.tail.id = backId  THEN
               -- lastSection := t1;
            -- else
               -- lastSection := t2;
            -- end if;
            
            -- -- Free last section and remove it from blocking lists
            -- -- Send section state message
            -- -- Remove sensor from back of train
            -- -- Tell train back sensor fired
            -- lastSection.state := free;
            -- lastSection.trainId := 0;
            -- releaseBlockings(lastSection.blockingList);
            -- SendToOutQueue(makePutSectionStateMsg(lastSection.Id, Free));
            -- RemoveLastSensor(TrainId);
            -- SendToTrainQueue(makeBackSensorFiredMsg(TrainId), TrainId);

            -- PutTrainPositionMsg(TrainId);
        
            -- -- Tell all trains to try to move again
            -- SendToAllTrainQueues(makeTryToMoveAgainMsg);         
            
            -- disposeSensorArray(sensorsPtr);
            -- return;
            
         -- elsif sensorId = sensorsPtr(sensorsPtr'last) then
         
            -- if oldSensorState = open then
               -- -- Logic error if open --> closed
               -- myPutLine("      xxxxxxxxxxxxx: LOBIC ERROR sn open --> closed " & integer'image(sensorId) ); 
               -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
            -- else
               -- -- Okay if closed --> open
               -- myPutLine("      xxxxxxxxxxxxx: back of train leaving sensor " & integer'image(sensorId));             
            -- end if;

            -- disposeSensorArray(sensorsPtr);
            -- return;
            
         -- else
         
            -- -- Undetermined logic error
            -- myPutLine("      xxxxxxxxxxxxx: UNDETERMINED LOGIC ERROR on sensor " & integer'image(sensorId) ); 
            -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
            -- disposeSensorArray(sensorsPtr);
            -- return;            

         -- end if;
         
      -- EXCEPTION
         -- WHEN Error : OTHERS =>
            -- put_line("**************** EXCEPTION Layout pkg in IdentifyTrainV2: " & Exception_Information(Error));
            -- myPutLine("    sensor id #" & Positive'Image(sensorId));
            -- RAISE;
      -- END IdentifyTrainV2;

      ---vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ---vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ---vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
         procedure putTrainPositionMsg(TrainId : trainIdType) is
            sensorsPtr : sensorArrayAccess;
            sList        : naturalListType;
         begin
            sensorsPtr := GetSensors(TrainId);
            convertSensorArrayToList(sensorsPtr, sList); 
            SendToOutQueue(makePutTrainPositionMsg(TrainId, sList)); 
            makeEmpty(sList);
            disposeSensorArray(sensorsPtr);
         end putTrainPositionMsg;

      PROCEDURE IdentifyTrainV1 (SensorID : Positive) IS
         SensorPtr          : SensorNodePtr;
         section1           : sectionNodePtr;
         section2           : sectionNodePtr;
         -- nextFreeSection    : sectionNodePtr;
         section            : sectionNodePtr;
         sectionNode        : sectionNodePtr;
         s1, sn         : sensorNodePtr;
         -- sf front of reserved section, s1 front of train, sn back of train
         oldSensorState     : sensorStateType;
         newSensorState     : sensorStateType;
         trainId             : trainIdType;
         searchOutcome      : natural;
         SensorPtrs         : accessArraySensorNodePtrType;
         last               : positive;
      BEGIN
         FindSensor(SensorList, SensorID, SensorPtr);
         
         IF SensorPtr = NULL THEN
            myPutLine("      -------------IdentifyTrainV1: ERROR(maybe) sensor not recognized " & integer'image(sensorId) ); 
            return;
         end if;
         
         oldSensorState := sensorPtr.state;
         flipSensor(sensorPtr);
         newSensorState := sensorPtr.state;
         if oldSensorState = open then
            myPutLine("      -------------IdentifyTrainV1: sensor " & integer'image(sensorId) & " open-->closed");   
         else
            myPutLine("      -------------IdentifyTrainV1: sensor " & integer'image(sensorId) & " closed-->open");   
         end if;
         
         -- Inform Othrottles that sensor has fired and its new state                 
         SendToOutQueue(makePutSensorStateMsg(SensorId, newSensorState));
         
         -- if not Simulator and Clock - sensorPtr.StartTime < 1.0 then
            -- myPutLine("      -------------IdentifyTrainV1: ERROR(maybe) sensor fired too recently " & integer'image(sensorId) ); 
            -- return;
         -- end if;
                     
         GetOccResSections(sensorId, section1, section2, searchOutcome);
         trainId := 0;
         if section1 /= null then 
            trainId := section1.trainId;
            myPutLine("      -------------IdentifyTrainV1: section1/state/train1 are " & 
                      natural'image(section1.id) & "/" & 
                      sectionStateType'image(section1.state) & "/" &
                      natural'image(trainId));   
         end if;
         if section2 /= null then 
            myPutLine("      -------------IdentifyTrainV1: section2/state/train2 are " & 
                      natural'image(section2.id) & "/" & 
                      sectionStateType'image(section2.state) & "/" &
                      natural'image(section2.trainId));   
         end if;
            
         IF searchOutcome = 1 THEN
         
            --  case 1: neither section occupied/reserved         null       /  null         
            myPutLine("      -------------IdentifyTrainV1: C1 MYSTERY SENSOR FIRING not close to any trains: error stop all" );
            SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            
         ELSIF searchOutcome = 2 THEN
         
            --  case 2: only one section occupied/reserved        not null   /  null
            sn := getBackSensorPtr(trainId);
            s1 := getFrontSensorPtr(trainId);
            s1.state := open;
            SendToOutQueue(makePutSensorStateMsg(s1.id, open));
            if section1.state = occupied and sensorId = sn.id and oldSensorState = closed then
               myPutLine("      -------------IdentifyTrainV1: C2 NORMAL back of train leaving closed sensor sn"); 
            elsif section1.state = occupied and sensorId = sn.id and oldSensorState = open then
               myPutLine("      -------------IdentifyTrainV1: C2 ERROR back of train leaving open sensor. Error stop train.");
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);      
               -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            elsif section1.state = occupied and sensorId = s1.id then 
               myPutLine("      -------------IdentifyTrainV1: C2 ERROR no reserved section but s1 fired anyway.  Error stop train.");
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);      
            elsif section1.state = reserved then
               
               -- In this section of code we now use s1.id instead of sf = sensorId
               if oldSensorState = closed then
                  myPutLine("      -------------IdentifyTrainV1: C2 ERROR sf closed-->open should have handled this when open-->closed. Error stop train");
               else
                  myPutLine("      -------------IdentifyTrainV1: C2 Just reached sf, we want to open s1 and handle s1 closed-->open");
                  myPutLine("      -------------IdentifyTrainV1: C2 NORMAL front of train leaving sensor s1.");      
                  IF section1.State = Reserved THEN
                     section1.State := Occupied;
                     SendToOutQueue(makePutSectionStateMsg(section1.Id, Occupied));
                     IF section1.SensorList.head.id = s1.id THEN                
                        AddNewSensorToFront(section1.TrainId, section1.SensorList.tail);   
                     ELSE
                        AddNewSensorToFront(section1.TrainId, section1.SensorList.head);
                     END IF;
                  ELSE
                     section2.State := Occupied;
                     SendToOutQueue(makePutSectionStateMsg(section2.Id, Occupied));
                     IF section2.SensorList.head.id = s1.id THEN               
                        AddNewSensorToFront(section2.TrainId, section2.SensorList.tail);
                     ELSE
                        AddNewSensorToFront(section2.TrainId, section2.SensorList.head);
                     END IF;
                  END IF;
                  trainId := section1.trainId;
                  if countSensors(trainId) > kMaxTrainLength + 1 then
                     myPutLine("      -------------IdentifyTrainV1: C5 ERROR train " & integer'image(trainId) & 
                               " too long. Error stop train");   
                     sendToTrainQueue(makeSensorErrorMsg(s1.id), trainId);                     
                  else   
                     SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);  -- Tell train front sensor has fired
                  end if;
                  PutTrainPositionMsg(TrainId);                                    -- Put train position
               end if;
               
               -- sf := getSensorAtFrontOfReservedSectionPtr(section1.all); 
               -- if oldSensorState = open then
                  -- myPutLine("      -------------IdentifyTrainV1: C2 IGNORE front of train approaching sf. Fix when leaving");
               -- else
                  -- myPutLine("      -------------IdentifyTrainV1: C2 FIXING front of train leaving sf.");   
                  -- section1.state := occupied;
                  -- SendToOutQueue(makePutSectionStateMsg(section1.Id, Occupied));
                  -- getFreeSection(section1.nextSectionList, nextFreeSection);
                  -- if nextFreeSection = null then 
                     -- getFreeSection(section1.prevSectionList, nextFreeSection);
                  -- end if;
                  -- if nextFreeSection = null then
                     -- myPutLine("      -------------IdentifyTrainV1: C2 ERROR couldn't fix, next section blocked. Error stop train");   
                     -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);      
                  -- else 
                     -- nextFreeSection.state := occupied;
                     -- BlockSections(nextFreeSection.BlockingList);
                     -- nextFreeSection.trainId := trainId;
                     -- SendToOutQueue(makePutSectionStateMsg(nextFreeSection.Id, Occupied));
                     -- s1.state := open;   -- Set s1 open for safety
                     -- SendToOutQueue(makePutSensorStateMsg(s1.id, open));
                     -- if section1.sensorList.head.id = sf.id then    -- Add sf to front of train
                        -- addNewSensorToFront(trainId, section1.sensorList.head);  
                     -- else
                        -- addNewSensorToFront(trainId, section1.sensorList.tail);
                     -- end if;
                     -- if nextFreeSection.sensorList.head.id = sf.id then   -- Add sf+1 to front of train
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.tail);
                     -- else
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.head);
                     -- end if;
                     -- if countSensors(trainId) > kMaxTrainLength + 1 then
                        -- myPutLine("      -------------IdentifyTrainV1: C2 ERROR train " & integer'image(trainId) & 
                                  -- " too long. Error stop train");   
                        -- sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);                     
                     -- else   
                        -- SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);  -- Tell train front sensor has fired
                     -- end if;
                     -- PutTrainPositionMsg(TrainId);                                    -- Put train position
                  -- end if;
               -- end if;
               
            else
               myPutLine("      -------------IdentifyTrainV1: C2 ERROR no clue what went wrong. Error stop train");
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);                     
               -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            end if;
   
         elsif searchOutcome = 3 then
         
            --  case 3: both sections occupied/reserved but with 
            --          different trainId's                       not null   /  not null
            if sensorId /= getBackSensor(section1.trainId)
            and sensorId /= getBackSensor(section2.trainId) then
               myPutLine("      -------------IdentifyTrainV1: C3 ERROR doesn't match back of either train. Error stop both trains.");
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               sendToTrainQueue(makeSensorErrorMsg(SensorId), section2.trainId);
               -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            elsif oldSensorState = open then
               myPutLine("      -------------IdentifyTrainV1: C3 ERROR double occupancy, one train has run into another. Error stop both trains. ");
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               sendToTrainQueue(makeSensorErrorMsg(SensorId), section2.trainId);
               -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            elsif oldSensorState = closed then 
               myPutLine("      -------------IdentifyTrainV1: C3 NORMAL back of train leaving closed sensor sn"); 
            else
               myPutLine("      -------------IdentifyTrainV1: C3 ERROR no clue what went wrong. Error stop both trains. ");
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               sendToTrainQueue(makeSensorErrorMsg(SensorId), section2.trainId);
               -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            end if;
            
         elsif searchOutcome = 4 then
         
            --  case 4: both sections occupied with      
            --          same trainId                              not null   /  not null
            if not sensorUnderTrain(trainId, sensorId) then
               myPutLine("      -------------IdentifyTrainV1: C4 ERROR sensor not under train. Error stop train."); 
               sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
               -- SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            else -- sensor = sn-1, sn-2, ..., s2
            
               SensorPtrs := GetSensorPtrs(TrainId); -- This array contains pointers to s1,s2,...,sn
               last := sensorPtrs'last;
               
               if oldSensorState = closed then
                  myPutLine("      -------------IdentifyTrainV1: C4 FIXING sensor unexpectedly closed, flip, and continue"); 
                  flipSensor(sensorPtr);
                  SendToOutQueue(makePutSensorStateMsg(SensorId, closed));
               end if;
               -- NORMAL from here if sn-1 fired else fixing by removing extra sensorStateType
               for i in reverse 2..last loop
                  exit when sensorId = sensorPtrs(i).id;
                  myPutLine("      -------------IdentifyTrainV1: C4 removing sensor"  
                            & integer'image(sensorPtrs(i).id) & " from back of train"); 
                  sensorPtrs(i).state := open;   -- Open si for safety
                  SendToOutQueue(makePutSensorStateMsg(sensorPtrs(i).id, open));               
                  getSection(sectionNode, sensorPtrs(i-1).id, sensorPtrs(i).id);
                  section := sectionNode;
                  section.state := free;
                  SendToOutQueue(makePutSectionStateMsg(section.Id, Free));
                  ReleaseBlockings(section.BlockingList);
                  section.trainId := 0;
                  RemoveLastSensor(TrainId);
                  SendToTrainQueue(makeBackSensorFiredMsg(TrainId), TrainId);
               end loop;   
               putTrainPositionMsg(TrainId);
               SendToAllTrainQueues(makeTryToMoveAgainMsg);               
               disposeArrayOfsensorNodePtr(sensorPtrs);            
            end if;

         elsif searchOutcome = 5 then
               
            -- case 5:  one section occupied, one reserved with
            --          same train id                             not null  /  not null 
            if oldSensorState = open then
               myPutLine("      -------------IdentifyTrainV1: C5 NORMAL front of train approaching sensor s1, ignore.");      
            else 
               myPutLine("      -------------IdentifyTrainV1: C5 NORMAL front of train leaving sensor s1.");      
               IF section1.State = Reserved THEN
                  section1.State := Occupied;
                  SendToOutQueue(makePutSectionStateMsg(section1.Id, Occupied));
                  IF section1.SensorList.head.id = sensorId THEN
                     AddNewSensorToFront(section1.TrainId, section1.SensorList.tail);   
                  ELSE
                     AddNewSensorToFront(section1.TrainId, section1.SensorList.head);
                  END IF;
               ELSE
                  section2.State := Occupied;
                  SendToOutQueue(makePutSectionStateMsg(section2.Id, Occupied));
                  IF section2.SensorList.head.id = sensorId THEN
                     AddNewSensorToFront(section2.TrainId, section2.SensorList.tail);
                  ELSE
                     AddNewSensorToFront(section2.TrainId, section2.SensorList.head);
                  END IF;
               END IF;
               trainId := section1.trainId;
               if countSensors(trainId) > kMaxTrainLength + 1 then
                  myPutLine("      -------------IdentifyTrainV1: C5 ERROR train " & integer'image(trainId) & 
                            " too long. Error stop train");   
                  sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);                     
               else   
                  SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);  -- Tell train front sensor has fired
               end if;
               PutTrainPositionMsg(TrainId);                                    -- Put train position
            end if;
         end if;
         
         return;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IdentifyTrainV1: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END IdentifyTrainV1;
      
      --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      PROCEDURE MakeReservation (
            TrainId :        TrainIdType;
            Result  :    OUT Boolean) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
         Sensors    : SensorNodeList;
         OutSectPtr : sectionNodePtr;
      BEGIN
         Result := False;
         GetTrainSensorList(TrainId, Sensors);
         IF Sensors.Head = NULL THEN
            RETURN;
         END IF;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Occupied AND sectionPtr.trainId = TrainId THEN
               IF SectionPtr.SensorList.head.id = Sensors.head.id THEN
                  GetFreeSection(SectionPtr.PrevSectionList, OutSectPtr);
               ELSIF SectionPtr.SensorList.tail.id = Sensors.head.id THEN
                  GetFreeSection(SectionPtr.NextSectionList, OutSectPtr);
               END IF;
               IF OutSectPtr /= NULL THEN
                  OutSectPtr.State := Reserved;
                  OutSectPtr.TrainId := TrainId;
                  BlockSections(OutSectPtr.BlockingList);
                  Result := True;
                  SendToOutQueue(makePutSectionStateMsg(OutSectPtr.Id, Reserved));
                  RETURN;
               END IF;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MakeReservation: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END MakeReservation;
      
      procedure MakeSectionUseable   (sensor1 : positive; sensor2 : positive) is 
         -- sectionNPtr :  SectionNodePtr
      begin
         null;
         -- This is a preliminary simplified version
         -- Move all switches in the section to their proper positions and send
         -- a response message back to all OThrottles.
         
         -- 1) Find the section defined by the sensors
         -- findSection(sensor1, sensor2, sectionNPtr);
         -- swNPtr := sectionNPtr.section.switchList.head;
         -- while swNPtr /= null loop
            
         
         -- 2) Move each switch in the section
         
         
         
         
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MakeSectionUseable: " & Exception_Information(Error));
            put_line("    sensor1/sensor2" & integer'image(sensor1) & integer'image(sensor2));
            raise;
      END MakeSectionUseable;      

      PROCEDURE MoveNextSwitch (
            TrainId : TrainIdType;
            State   : SwitchStateType) IS
         TrainPtr      : trainNodePtr    := TrainList;
         SectionPtr    : SectionNodePtr;
         FrontSensorId : Positive;
         Result        : Boolean;
         switchPtr     : switchNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               GetSection(SectionPtr, TrainPtr.SensorList.head.id, TrainPtr.SensorList.Head.Next.id);
               IF SectionPtr /= NULL THEN
                  FrontSensorId := TrainPtr.SensorList.head.id;
                  LOOP
                     IF SectionPtr.SensorList.head.id = FrontSensorId THEN
                        SectionPtr := SectionPtr.PrevSectionList.Head;
                     ELSE
                        SectionPtr := SectionPtr.NextSectionList.Head;
                     END IF;
                     EXIT WHEN SectionPtr.SwitchList.Head /= NULL;
                     IF SectionPtr.SensorList.head.id = FrontSensorId THEN
                        FrontSensorId := SectionPtr.SensorList.tail.id;
                     ELSE
                        FrontSensorId := SectionPtr.SensorList.head.id;
                     END IF;
                  END LOOP;
                  
                  switchPtr := SectionPtr.SwitchList.Head;
                  if switchPtr.state = state or else
                     (switchPtr.state = beginThrown and state = thrown) or else
                     (switchPtr.state = beginClosed and state = closed)        
                  then
                     return;
                  end if;
                                  
                  MoveSwitchPossible(SectionPtr.SwitchList.Head, TrainId, Result);
                  IF Result THEN
                     IF State = Thrown THEN
                        SectionPtr.SwitchList.Head.State := BeginThrown;
                     ELSE
                        SectionPtr.SwitchList.Head.State := BeginClosed;
                     END IF;
                     SendLoseReservationMessages(SectionPtr.SwitchList.Head);          -- mo 1/8/12
                     SendToOutQueue(makePutSwitchStateMsg(SectionPtr.SwitchList.Head.Id, SectionPtr.SwitchList.Head.State));
                     SendToOutQueue(makeSwReqMsg(SectionPtr.SwitchList.Head.Id, State));
                  END IF;
               END IF;
               return;                   -- mo 1/31/12
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveNextSwitch: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END MoveNextSwitch;

      PROCEDURE MoveSwitch (SwitchId : Positive; State    : SwitchStateType) IS
         SwitchPtr            : SwitchNodePtr := SwitchList.Head;
         Result               : Boolean;
         trainId              : trainIdType;
         thereIsReservation   : boolean;
         found                : boolean;
      BEGIN
         while switchPtr /= null loop
            found := true;
            exit when switchPtr.id = SwitchId;
            found := false;
            SwitchPtr := SwitchPtr.Next;
         end loop;
         if not found or else
           switchPtr.state = state or else
           (switchPtr.state = beginThrown and state = thrown) or else
           (switchPtr.state = beginClosed and state = closed)        
         then
            return;
         end if;
         
         MoveSwitchPossible(SwitchPtr, Result);
         IF Result THEN
            FindIdOfTrainLoosingReservation(switchPtr, trainId, thereIsReservation);
            if thereIsReservation then
               SendToTrainQueue(makeLoseReservationMsg(TrainId), TrainId);
               -- The train delays when it does a "lose reservation"
               -- This causes the next two messages to be delayed until the train stops.
               sendToTrainQueue(makeSwReqMsg(switchPtr.id, State), trainId);
            else
               IF State = Thrown THEN
                  switchPtr.state := BeginThrown;
               ELSE
                  switchPtr.state := BeginClosed;
               END IF;
               SendToOutQueue(makePutSwitchStateMsg(switchPtr.id, switchPtr.state));
               SendToOutQueue(makeSwReqMsg(switchPtr.id, State));
            end if;
         END IF;

      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveSwitch: " & Exception_Information(Error));
            put_line("    switchId" & integer'image(switchId));
            raise;
      END MoveSwitch;

      -- Figure out the sections
      -- If it is possible
      --   place the trains
      --   add the train to the train list
      --   block the sections that need blocking
      PROCEDURE PositionTrain (
            TrainId :        TrainIdType;
            Count   :        Positive;
            Sensors :        SensorArrayType;
            Result  :    OUT Boolean) IS
         OutSectList : sectionNodeList;
         SectionPtr  : SectionNodePtr;
      BEGIN
         IF Sensors'Length /= Count OR Count < 2 THEN
            result := false;
            RETURN;
         END IF;
         FindAllSections(OutSectList, Sensors);
         IF not AllFree(OutSectList) THEN
            result := false;
            return;
         else
            PlaceTrainInSections(OutSectList, TrainId);
            
            if isNewTrain(trainId) then                          -- mo 1/18/12
               AddNewTrain(TrainId, Sensors);
            else
               UpdateTrainSensors(trainId, sensors);
            end if;            
            
            SectionPtr := OutSectList.Head;
            WHILE SectionPtr /= NULL LOOP
               BlockSections(SectionPtr.BlockingList);
               SectionPtr := SectionPtr.Next;
            END LOOP;
            Result := True;
            return;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in PositionTrain: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END PositionTrain;

      PROCEDURE ReleaseReservation (
            TrainId : TrainIdType) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Reserved AND
                  sectionPtr.trainId = TrainId THEN
               sectionPtr.state := Free;
               sectionPtr.trainId := 0;
               ReleaseBlockings(SectionPtr.BlockingList);
               SendToOutQueue(makePutSectionStateMsg(SectionPtr.Id, Free));
               SendToAllTrainQueues(makeTryToMoveAgainMsg);
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in ReleaseReservation: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END ReleaseReservation;

      procedure removeFromTrainList(trainId : TrainIdType) is
         prev, curr      : trainNodePtr;
      begin
         prev := null;
         curr := trainList;
         while curr /= null loop
            if curr.trainId /= trainId then
               prev := curr;
               curr := curr.next;
            else
               if curr = trainList then
                  trainList := trainList.next;    
               else
                  prev.next := curr.next;        
               end if;
               return;
            end if;
         end loop;
         return;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in removeFromTrainList: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END removeFromTrainList;            

      PROCEDURE repositionTrain (                                            -- mo 1/17/12
            TrainId :        TrainIdType;
            Count   :        Positive;
            Sensors :        SensorArrayType;
            Result  :    OUT Boolean) IS        
      begin
         freeAllSectionsOccupiedOrReservedByTrain(trainId);
         positionTrain(trainId, count, sensors, result);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in repositionTrain: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      end repositionTrain;

      procedure setAllSensorsOpen is 
         ptr : SensorNodePtr;
      begin
         ptr := sensorList.head;
         while ptr /= null loop
            if ptr.state = closed then
               ptr.state := open;
               SendToOutQueue(makePutSensorStateMsg(ptr.id, open));
            end if;
            ptr := ptr.next;
         end loop;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SetAllSensorsOpen: " & Exception_Information(Error));
            RAISE;
      end setAllSensorsOpen;

      PROCEDURE SwitchFinishedMoving (
            SwitchId : Positive;
            State    : SwitchStateType) IS
         SwitchPtr         : SwitchNodePtr  := SwitchList.Head;
         ThrownSectionList : sectionNodeList;
         ClosedSectionList : sectionNodeList;
         ThrownUseable     : Boolean        := False;
         ClosedUseable     : Boolean        := False;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            IF switchPtr.id = SwitchId THEN
               IF State = Closed THEN
                  switchPtr.state := Closed;
               ELSIF State = Thrown THEN
                  switchPtr.state := Thrown;
               END IF;
               SendToOutQueue(makePutSwitchStateMsg(switchPtr.id, switchPtr.state));
               SendToAllTrainQueues(makeTryToMoveAgainMsg);
               RETURN;
            END IF;
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line(" **************** EXCEPTION Layout pkg in SwitchFinishedMoving:" & Exception_Information(Error));
            put_line("    switchId" & integer'image(switchId));
            raise;
      END SwitchFinishedMoving;
      
-- End functions to manipulate data structures    
----------------------- 2a --------------------------------
----------------------- 2a --------------------------------
----------------------- 2a --------------------------------


   
------------------------------- 2b -----------------------------------------
------------------------------- 2b -----------------------------------------
------------------------------- 2b -----------------------------------------
-------------------- Begin build data structures from XML ------------------

      PROCEDURE bldNewSection (
            Id : Positive) IS
      BEGIN
         CurrentSection := NEW sectionNode;
         CurrentSection.Id := Id;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldNewSection: " & Exception_Information(Error));
            RAISE;
      END bldNewSection;

      PROCEDURE bldEndSection IS
      BEGIN
         IF SectionList.Head = NULL THEN
            SectionList.Head := NEW SectionNode;
            SectionList.Tail := SectionList.Head;
         ELSE
            SectionList.Tail.Next := NEW SectionNode;
            SectionList.Tail := SectionList.Tail.Next;
         END IF;
         SectionList.Tail := NEW sectionNode;
         SectionList.Tail.All := CurrentSection.All;
         Free_Section(CurrentSection);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldEndSection: " & Exception_Information(Error));
            RAISE;
     END bldEndSection;

      -- Add a section to the end of a section list
      PROCEDURE AddToEnd (
            OutSectList : IN OUT sectionNodeList;
            NodePtr     :        SectionNodePtr) IS
      BEGIN
         IF OutSectList.Head = NULL THEN
            OutSectList.Head := NEW SectionNode;
            OutSectList.Tail := OutSectList.Head;
         ELSE
            OutSectList.Tail.Next := NEW SectionNode;
            OutSectList.Tail := OutSectList.Tail.Next;
         END IF;
         OutSectList.Tail := NodePtr;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddToEnd: " & Exception_Information(Error));
            RAISE;
      END AddToEnd;

      -- check if a section is in a blocking list
      FUNCTION IsIn (
            BlockList : blockingSectionList;
            Id        : Positive)
        RETURN Boolean IS
         BlockingPtr : blockingSectionPtr := BlockList.Head;
      BEGIN
         WHILE BlockingPtr /= NULL LOOP
            IF BlockingPtr.Id = Id THEN
               RETURN True;
            END IF;
            BlockingPtr := BlockingPtr.Next;
         END LOOP;
         RETURN False;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IsIn: " & Exception_Information(Error));
            RAISE;
      END IsIn;

      PROCEDURE FindAllSections (
            SensorId         :        Positive;
            CurrentSectId    :        Positive;
            CurSectBlockList :        blockingSectionList;
            OutSectList      :    OUT sectionNodeList) IS
         SectionPtr :  SectionNodePtr := SectionList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            
               IF SectionPtr.Id /= CurrentSectId AND
                     NOT IsIn(CurSectBlockList, SectionPtr.Id) AND
                     SectionPtr.SensorList.Head /= NULL THEN
                  IF SectionPtr.SensorList.head.id =
                        SensorId OR
                        SectionPtr.SensorList.tail.id =
                        SensorId THEN
                     AddToEnd(OutSectList, SectionPtr);
                  END IF;
               END IF;
 
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindAllSections: " & Exception_Information(Error));
            RAISE;
      END FindAllSections;

      -- Set the NextSectionList for each section
      PROCEDURE SetNextSectionList IS
         SectionPtr     : SectionNodePtr := SectionList.Head;
         OutSectionList : sectionNodeList;
      BEGIN
         WHILE SectionPtr/= NULL LOOP
            FindAllSections(SectionPtr.SensorList.tail.id,
               SectionPtr.Id, SectionPtr.BlockingList,
               OutSectionList);
            SectionPtr.NextSectionList := OutSectionList;
            OutSectionList.Head := NULL;
            OutSectionList.Tail := NULL;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SetNextSectionList: " & Exception_Information(Error));
            RAISE;
      END SetNextSectionList;

      -- Set the PrevSectionList for each section
      PROCEDURE SetPrevSectionList IS
         SectionPtr     : SectionNodePtr := SectionList.Head;
         OutSectionList : sectionNodeList;
      BEGIN
         WHILE SectionPtr/= NULL LOOP
            FindAllSections(SectionPtr.SensorList.head.id,
               SectionPtr.Id, SectionPtr.BlockingList,
               OutSectionList);
            SectionPtr.PrevSectionList := OutSectionList;
            OutSectionList.Head := NULL;
            OutSectionList.Tail := NULL;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SetPrevSectionList: " & Exception_Information(Error));
            RAISE;
      END SetPrevSectionList;

      PROCEDURE bldEndSectionList IS
      BEGIN
         SetNextSectionList;
         SetPrevSectionList;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldEndSectionList: " & Exception_Information(Error));
            RAISE;
      END bldEndSectionList;

      FUNCTION CheckSensors (
            Sensors  : SensorNodeList;
            FirstId  : Positive;
            SecondId : Positive)
        RETURN Boolean IS
      BEGIN
         RETURN (Sensors.head.id = FirstId AND Sensors.tail.id = SecondId) OR
            (Sensors.tail.id = FirstId AND Sensors.head.id = SecondId);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in CheckSensors: " & Exception_Information(Error));
            raise;
      END CheckSensors;

      PROCEDURE GetSections (
            SwitchPtr  :        switchNodePtr;
            ThrownList :    OUT sectionNodeList;
            ClosedList :    OUT sectionNodeList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
         SensorPtr  : SensorNodePtr;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            CASE SwitchPtr.TypeOfSwitch IS
               WHEN Normal =>
                  IF CheckSensors(SectionPtr.SensorList, SwitchPtr.NarrowSensors.head.id, SwitchPtr.ThrownSensor.Id) THEN
                     AddToEnd(ThrownList, SectionPtr);
                  ELSE
                     SensorPtr := SwitchPtr.ClosedSensors.Head;
                     WHILE SensorPtr /= NULL LOOP
                        IF CheckSensors(SectionPtr.SensorList, SwitchPtr.NarrowSensors.head.id, SensorPtr.id) THEN
                           AddToEnd(ClosedList, SectionPtr);
                           SensorPtr := NULL;
                        ELSE
                           SensorPtr := SensorPtr.Next;
                        END IF;
                     END LOOP;
                  END IF;
               WHEN Crossover =>
                  IF CheckSensors(SectionPtr.SensorList, SwitchPtr.NarrowSensors.head.id, SwitchPtr.NarrowSensors.tail.id) THEN
                     AddToEnd(ThrownList, SectionPtr);
                  ELSIF CheckSensors(SectionPtr.SensorList, SwitchPtr.NarrowSensors.head.id, SwitchPtr.ClosedSensors.head.id) OR
                        CheckSensors(SectionPtr.SensorList, SwitchPtr.NarrowSensors.tail.id, SwitchPtr.ClosedSensors.tail.id) THEN
                     AddToEnd(ClosedList, SectionPtr);
                  END IF;
            END CASE;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetSections: " & Exception_Information(Error));
            RAISE;
      END GetSections;

      -- Finish things up when the SwitchList is all read in
      PROCEDURE bldEndSwitchList IS
         SwitchPtr         : SwitchNodePtr  := SwitchList.Head;
         ClosedSectionList : sectionNodeList;
         ThrownSectionList : sectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            GetSections(switchPtr, ThrownSectionList, ClosedSectionList);
            IF switchPtr.state = Closed THEN
               SectionPtr := ThrownSectionList.Head;
               WHILE SectionPtr /= NULL LOOP
                  SectionPtr.IsUseable := False;
                  SectionPtr := SectionPtr.Next;
               END LOOP;
            ELSE
               SectionPtr := ClosedSectionList.Head;
               WHILE SectionPtr /= NULL LOOP
                  SectionPtr.IsUseable := False;
                  SectionPtr := SectionPtr.Next;
               END LOOP;
            END IF;
                       
            if switchPtr.state = Read then
               SendToOutQueue(makeSwStateMsg(switchPtr.id));
            else
               SendToOutQueue(makeSwReqMsg(switchPtr.id, switchPtr.state));
               if not simulator then 
                  null;
                  -- delay 3.0;             -- don't want to overwhelm the locobuffer server
               end if;
               if  switchPtr.state = Closed then
                  SendToOutQueue(makePutSwitchStateMsg(switchPtr.id, BeginClosed));
               else
                  SendToOutQueue(makePutSwitchStateMsg(switchPtr.id, BeginThrown));
               end if;
            end if;
            ClosedSectionList.Head := NULL;
            ClosedSectionList.Tail := NULL;
            ThrownSectionList.Head := NULL;
            ThrownSectionList.Tail := NULL;
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldEndSwitchList: " & Exception_Information(Error));
            RAISE;
      END bldEndSwitchList;

      PROCEDURE FindSwitch (
            Switchs   :        SwitchNodeList;
            SwitchId  :        Positive;
            SwitchPtr :    OUT SwitchNodePtr) IS
      BEGIN
         SwitchPtr := Switchs.Head;
         WHILE SwitchPtr /= NULL LOOP
            IF switchPtr.id = SwitchId THEN
               RETURN;
            END IF;
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindSwitch: " & Exception_Information(Error));
            put_line("    Looking for switch # " & Positive'Image(SwitchId));
            RAISE;
      END FindSwitch;

      PROCEDURE bldAddSwitch (
            Id    : Positive) IS
         SwitchPtr : SwitchNodePtr;
      BEGIN
         IF CurrentSection.SwitchList.Head = NULL THEN
            CurrentSection.SwitchList.Head := NEW SwitchNode;
            CurrentSection.SwitchList.Tail :=
               CurrentSection.SwitchList.Head;
         ELSE
            CurrentSection.SwitchList.Tail.Next := NEW SwitchNode;
            CurrentSection.SwitchList.Tail :=
               CurrentSection.SwitchList.Tail.Next;
         END IF;

         FindSwitch(SwitchList, Id, SwitchPtr);
         IF SwitchPtr = NULL THEN
            IF SwitchList.Head = NULL THEN
               SwitchList.Head := NEW SwitchNode;
               SwitchList.Tail := SwitchList.Head;
            ELSE
               SwitchList.Tail.Next := NEW SwitchNode;
               SwitchList.Tail := SwitchList.Tail.Next;
            END IF;
            SwitchPtr := CurrentSection.SwitchList.Tail;
            switchPtr := new switchNode;
            switchPtr.id := Id;
            switchList.tail := switchPtr;
         ELSE
            CurrentSection.switchList.tail := switchPtr;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldAddSwitch: " & Exception_Information(Error));
            put_line("    adding switch #" & Positive'Image(Id));
            RAISE;
      END bldAddSwitch;

      PROCEDURE bldUpdateSwitch (
            Id           : Positive;
            TypeOfSwitch : ControllerGlobals.SwitchType;
            state        : switchStateType) IS
         SwitchPtr : SwitchNodePtr;
      BEGIN
         FindSwitch(SwitchList, Id, SwitchPtr);
         IF SwitchPtr /= NULL THEN
            CurrentSwitch := switchPtr;
            CurrentSwitch.TypeOfSwitch := TypeOfSwitch;
            currentSwitch.state := state;
         ELSE
            RAISE InvalidSwitchId;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldUpdateSwitch: " & Exception_Information(Error));
            put_line("    switch #" & Positive'Image(Id));
            RAISE;
      END bldUpdateSwitch;

      PROCEDURE bldUpdateSwitchNarrow (
            NarrowId : Positive) IS
         SensorPtr : SensorNodePtr;
      BEGIN
         IF CurrentSwitch /= NULL THEN
            FindSensor(SensorList, NarrowId, SensorPtr);
            IF SensorPtr /= NULL THEN
               IF CurrentSwitch.NarrowSensors.Head = NULL THEN
                  CurrentSwitch.NarrowSensors.Head := NEW SensorNode;
                  CurrentSwitch.NarrowSensors.Tail :=
                     CurrentSwitch.NarrowSensors.Head;
               ELSE
                  CurrentSwitch.NarrowSensors.Tail.Next := NEW SensorNode;
                  CurrentSwitch.NarrowSensors.Tail :=
                     CurrentSwitch.NarrowSensors.Tail.Next;
               END IF;
               CurrentSwitch.NarrowSensors.tail := sensorPtr;
            ELSE
               RAISE InvalidSensorId;
            END IF;
         ELSE
            RAISE CurrentSwitchNull;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldUpdateSwitchNarrow: " & Exception_Information(Error));
            put_line("    narrow id #" & Positive'Image(narrowId));
            RAISE;
      END bldUpdateSwitchNarrow;

      PROCEDURE bldUpdateSwitchClosed (
            ClosedId : Positive) IS
         SensorPtr : SensorNodePtr;
      BEGIN
         IF CurrentSwitch /= NULL THEN
            FindSensor(SensorList, ClosedId, SensorPtr);
            IF SensorPtr /= NULL THEN
               IF CurrentSwitch.ClosedSensors.Head = NULL THEN
                  CurrentSwitch.ClosedSensors.Head := NEW SensorNode;
                  CurrentSwitch.ClosedSensors.Tail :=
                     CurrentSwitch.ClosedSensors.Head;
               ELSE
                  CurrentSwitch.ClosedSensors.Tail.Next := NEW SensorNode;
                  CurrentSwitch.ClosedSensors.Tail :=
                     CurrentSwitch.ClosedSensors.Tail.Next;
               END IF;
               CurrentSwitch.ClosedSensors.tail := sensorPtr;
            ELSE
               RAISE InvalidSensorId;
            END IF;
         ELSE
            RAISE CurrentSwitchNull;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldUpdateSwitchClosed: " & Exception_Information(Error));
            put_line("    closed id #" & Positive'Image(closedId));
            RAISE;
      END bldUpdateSwitchClosed;

      PROCEDURE bldUpdateSwitchThrown (
            thrownId : Positive) IS
         SensorPtr : SensorNodePtr;
      BEGIN
         IF CurrentSwitch /= NULL THEN
            FindSensor(SensorList, thrownId, SensorPtr);
            IF SensorPtr /= NULL THEN
               CASE CurrentSwitch.TypeOfSwitch IS
                  WHEN Normal =>
                     CurrentSwitch.ThrownSensor := sensorPtr;
                  WHEN Crossover =>
                     RAISE InvalidSwitchType;
               END CASE;
            ELSE
               RAISE InvalidSensorId;
            END IF;
         ELSE
            RAISE CurrentSwitchNull;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldUpdateSwitchThrown: " & Exception_Information(Error));
            put_line("    thrown id #" & Positive'Image(thrownId));
            RAISE;
      END bldUpdateSwitchThrown;

      PROCEDURE bldAddSensor (Id : Positive) IS
         SensorPtr : SensorNodePtr;
      BEGIN
         IF CurrentSection.SensorList.Head = NULL THEN
            CurrentSection.SensorList.Head := NEW SensorNode;
            CurrentSection.SensorList.Tail := CurrentSection.SensorList.Head;
         ELSE
            CurrentSection.SensorList.Tail.Next := NEW SensorNode;
            CurrentSection.SensorList.Tail := CurrentSection.SensorList.Tail.Next;
         END IF;

         FindSensor(SensorList, Id, SensorPtr);
         IF SensorPtr = NULL THEN
            IF SensorList.Head = NULL THEN
               SensorList.Head := NEW SensorNode;
               SensorList.Tail := SensorList.Head;
            ELSE
               SensorList.Tail.Next := NEW SensorNode;
               SensorList.Tail := SensorList.Tail.Next;
            END IF;
            SensorPtr := CurrentSection.SensorList.Tail;
            sensorPtr := new sensorNode;
            SensorPtr.id := Id;
            SensorList.tail := sensorPtr;
         ELSE
            CurrentSection.SensorList.tail := sensorPtr;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldAddSensor: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(Id));
            RAISE;
      END bldAddSensor;

      PROCEDURE bldAddBlocking (
            Id : Positive) IS
      BEGIN
         IF CurrentSection.BlockingList.Head = NULL THEN
            CurrentSection.BlockingList.Head := NEW blockingSection;
            CurrentSection.BlockingList.Tail := CurrentSection.BlockingList.Head;
         ELSE
            CurrentSection.BlockingList.Tail.Next := NEW blockingSection;
            CurrentSection.BlockingList.Tail := CurrentSection.BlockingList.Tail.Next;
         END IF;
         CurrentSection.BlockingList.Tail.Id := Id;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldAddBlocking: " & Exception_Information(Error));
            put_line("    id #" & Positive'Image(Id));
            RAISE;
      END bldAddBlocking;
      
-------------------- End build data structures from XML --------------------
------------------------------- 2b -----------------------------------------
------------------------------- 2b -----------------------------------------
------------------------------- 2b -----------------------------------------
   

------------------------------- 2c ----------------------------------
------------------------------- 2c ----------------------------------
------------------------------- 2c ----------------------------------
----------------- Begin debug print data structures -----------------
-------              with helper functions              -------------

      PROCEDURE Print (
            OutStr : String;
            Indent : Natural;
            Output : File_Type) IS
      BEGIN
         FOR I IN 0..Indent LOOP
            put(Output, " ");
         END LOOP;
         put_line(Output, OutStr);
      END Print;

      PROCEDURE Print_Sensors (
            Sensors     : SensorNodeList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False) IS
         SensorPtr : SensorNodePtr := Sensors.Head;
      BEGIN
         WHILE SensorPtr /= NULL LOOP
            Print("Sensor ID: " & Positive'Image(SensorPtr.id),
               Indent, Output);
            IF NOT PrintOnlyId THEN
               Print("Sensor State: " & SensorStateType'Image(
                     SensorPtr.state), Indent, Output);
            END IF;
            SensorPtr := SensorPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in PrintSensors: " & Exception_Information(Error));
            RAISE;
      END Print_Sensors;

      PROCEDURE Print_Switchs (
            Switchs     : SwitchNodeList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False) IS
         SwitchPtr : SwitchNodePtr := Switchs.Head;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            Print("----------------------", Indent, Output);
            Print("Switch ID: " & Positive'Image(switchPtr.id),
               Indent, Output);
            IF NOT PrintOnlyId THEN
               Print("Switch State: " & SwitchStateType'Image(switchPtr.state), Indent, Output);
               Print("Switch Type: " & ControllerGlobals.SwitchType'Image(switchPtr.TypeOfSwitch), Indent, Output);
               Print("Switch Narrow Sensors:", Indent, Output);
               Print_Sensors(switchPtr.NarrowSensors, Indent + 2, Output, True);
               Print("Switch Closed Sensors:", Indent, Output);
               Print_Sensors(switchPtr.ClosedSensors, Indent + 2, Output, True);
               IF switchPtr.ThrownSensor /= NULL THEN
                  Print("Switch Thrown Sensor: " & Positive'Image(switchPtr.thrownSensor.id), Indent, Output);
               END IF;
            END IF;
            Print("----------------------", Indent, Output);
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in Print_Switches: " & Exception_Information(Error));
            RAISE;
      END Print_Switchs;

      PROCEDURE Print_Blockings (
            BlockingList : blockingSectionList;
            Indent       : Natural;
            Output       : File_Type) IS
         BlockingPtr : blockingSectionPtr := BlockingList.Head;
      BEGIN
         WHILE BlockingPtr /= NULL LOOP
            Print("Blocking ID: " & Positive'Image(BlockingPtr.Id), Indent, Output);
            BlockingPtr := BlockingPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in PrintBlockings: " & Exception_Information(Error));
            RAISE;
      END Print_Blockings;

      PROCEDURE Print_Sections (
            Sections    : sectionNodeList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean        := False) IS
         SectionPtr : SectionNodePtr := Sections.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            if printOnlyId then
               Print("Section ID: " & Positive'Image(SectionPtr.Id), Indent, Output);
            else
               Print("---------------", Indent, Output);
               Print("Section ID: " & Positive'Image(SectionPtr.Id), Indent, Output);
               Print("Section State: " & SectionStateType'Image(
                     sectionPtr.state), Indent, Output);
               Print("Section Is Useable: " & Boolean'Image(SectionPtr.IsUseable), Indent, Output);
               IF sectionPtr.state = Occupied OR sectionPtr.state = Reserved THEN
                  Print("Section TrainAddr: " & TrainIdType'Image(sectionPtr.trainId), Indent, Output);
               END IF;
               IF sectionPtr.state = Blocked THEN
                  Print("Section Block Count:" & Positive'Image(SectionPtr.BlockCount), Indent, Output);
               END IF;
               Print("Section Sensors:", Indent, Output);
               Print_Sensors(SectionPtr.SensorList, Indent + 2, Output, True);
               Print("Section Switchs:", Indent, Output);
               Print_Switchs(SectionPtr.SwitchList, Indent + 2, Output, True);
               Print("Next Sections:", Indent, Output);
               Print_Sections(SectionPtr.NextSectionList, Indent + 2, Output, True);
               Print("Previous Sections:", Indent, Output);
               Print_Sections(SectionPtr.PrevSectionList, Indent + 2, Output, True);
               Print("Section Blockings:", Indent, Output);
               Print_Blockings(SectionPtr.BlockingList, Indent + 2, Output);
               Print("---------------", Indent, Output);
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in Print_Sections: " & Exception_Information(Error));
            RAISE;
      END Print_Sections;

----------------- End debug print data structures ------------------
---------------------------- 2c ------------------------------------
---------------------------- 2c ------------------------------------
---------------------------- 2c ------------------------------------
            

-------------- 2d --------------------
-------------- 2d --------------------
-------------- 2d --------------------
------- Begin Get/Set Functions ------ 
     
      FUNCTION GetXMLFilename RETURN Unbounded_String IS
      BEGIN
         RETURN XMLFilename;
      END GetXMLFilename;

      PROCEDURE SetXMLFilename (Filename : Unbounded_String) IS
      BEGIN
         XMLFilename := Filename;
      END SetXMLFilename;
      
------- End Get/Set Functions---------  
------------- 2d ---------------------
------------- 2d ---------------------
------------- 2d ---------------------



--------------------------- 2f --------------------------------------
--------------------------- 2f --------------------------------------
--------------------------- 2f --------------------------------------
-------------------- Begin helper function ------------------------- 

      PROCEDURE FindSensor (
            Sensors   :        SensorNodeList;
            SensorId  :        Positive;
            SensorPtr :    OUT SensorNodePtr) IS
      BEGIN
         SensorPtr := Sensors.Head;
         WHILE SensorPtr /= NULL LOOP
            IF SensorPtr.id = SensorId THEN
               RETURN;
            END IF;
            SensorPtr := SensorPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindSensor: " & Exception_Information(Error));
            put_line("    Looking for sensor #" & Positive'Image(SensorId));
            RAISE;
      END FindSensor;

      PROCEDURE ReleaseBlockings (
            BlockingList : blockingSectionList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         IF BlockingList.Head /= NULL THEN
            WHILE SectionPtr /= NULL LOOP
               IF IsIn(BlockingList, SectionPtr.Id) THEN
                  SectionPtr.BlockCount := SectionPtr.BlockCount - 1;
                  IF SectionPtr.BlockCount = 0 THEN
                     sectionPtr.state := Free;
                     sectionPtr.trainId := 0;
                     SendToOutQueue(makePutSectionStateMsg(SectionPtr.Id, Free));
                  END IF;
               END IF;
               SectionPtr := SectionPtr.Next;
            END LOOP;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in ReleaseBlockings: " & Exception_Information(Error));
            RAISE;
      END ReleaseBlockings;
      
      -- Get a train's sensor numbers 
      function getTrainsSensorNumbers(trainId : trainIdType) return naturalListType is
         TrainPtr  : trainNodePtr   := TrainList;
         SensorPtr : SensorNodePtr := null;
         sList     : naturalListType;
      begin
         while trainPtr /= null loop
            if TrainPtr.TrainId = TrainId THEN
               sensorPtr := trainPtr.sensorList.head;
               exit;
            end if;
            trainPtr := trainPtr.next;
         end loop;
         makeEmpty(sList);
         while sensorPtr /= null loop
            addEnd(sList, sensorPtr.id);
            sensorPtr := sensorPtr.next;
         end loop;
         return sList;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getTrainsSensorNumbers: " & Exception_Information(Error));
            RAISE;
      end getTrainsSensorNumbers;
      
      -- Add a new sensor to the front of a train's sensor list
      PROCEDURE AddNewSensorToFront (TrainId : TrainIdType; Sensor  : sensorNodePtr) IS
         TrainPtr  : trainNodePtr   := TrainList;
         SensorPtr : SensorNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               myPutLine("      <<<<<<<<<<< " & integer'image(sensor.id)  & " sensor added to front");
               TrainPtr.SensorCount := TrainPtr.SensorCount + 1;
               SensorPtr := TrainPtr.SensorList.Head;
               TrainPtr.SensorList.Head := NEW SensorNode;
               TrainPtr.SensorList.head := Sensor;
               TrainPtr.SensorList.Head.Next := SensorPtr;
               RETURN;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddNewSensorToFront: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      END AddNewSensorToFront;
         
      FUNCTION GetSensorPtrs (TrainId : TrainIdType) RETURN accessArraySensorNodePtrType IS
         TrainPtr  : trainNodePtr       := TrainList;
         SensorPtr : SensorNodePtr;
         Sensors   : accessArraySensorNodePtrType;
         I         : Positive          := 1;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               Sensors := NEW arraySensorNodePtrType(1..TrainPtr.SensorCount);
               SensorPtr := TrainPtr.SensorList.Head;
               WHILE SensorPtr /= NULL LOOP
                  Sensors(I) := sensorPtr;
                  I := I + 1;
                  SensorPtr := SensorPtr.Next;
               END LOOP;
               
               -- myPutLine(" ");
               -- myPutLine("          In GetSensors");
               -- for i in sensors.all'range loop
                  -- myPutLine("          sensor " & integer'image(Sensors(i)));
               -- end loop; 
               -- myPutLine(" ");
               
               return sensors;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return null;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetSensorPtrs: " & Exception_Information(Error));
            raise;
      END GetSensorPtrs;    

      FUNCTION GetSensors (TrainId : TrainIdType) RETURN SensorArrayAccess IS
         TrainPtr  : trainNodePtr       := TrainList;
         SensorPtr : SensorNodePtr;
         Sensors   : SensorArrayAccess;
         I         : Positive          := 1;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               Sensors := NEW SensorArrayType(1..TrainPtr.SensorCount);
               SensorPtr := TrainPtr.SensorList.Head;
               WHILE SensorPtr /= NULL LOOP
                  Sensors(I) := SensorPtr.id;
                  I := I + 1;
                  SensorPtr := SensorPtr.Next;
               END LOOP;
               
               myPutLine(" ");
               myPutLine("          In GetSensors");
               for i in sensors.all'range loop
                  myPutLine("          sensor " & integer'image(Sensors(i)));
               end loop; 
               myPutLine(" ");
               
               return sensors;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return null;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetSensors: " & Exception_Information(Error));
            raise;
      END GetSensors;

      function countSensors(trainId : trainIdType) return natural is
         TrainPtr  : trainNodePtr       := TrainList;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               return trainPtr.sensorCount;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return 0;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in countSensors: " & Exception_Information(Error));
            raise;
      END countSensors;

      -- Get the two sections surrounding the sensor 
      PROCEDURE getUnbockedUsableSectionsContainingSensor (
            SensorID      :        Positive;
            FirstSection  :    OUT sectionNodePtr;
            SecondSection :    OUT sectionNodePtr) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         FirstSection := NULL;
         SecondSection := NULL;
         WHILE SectionPtr /= NULL LOOP
            IF      (sectionPtr.state /= blocked)
            AND THEN(SectionPtr.SensorList.head.id = SensorId OR
                     SectionPtr.SensorList.tail.id = SensorId)         
            AND THEN (IsSectionUseable(sectionPtr))
            THEN
               IF FirstSection = NULL THEN
                  FirstSection := sectionPtr;
               ELSE
                  SecondSection := sectionPtr;
                  RETURN;
               END IF;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getUnbockedUsableSectionsContainingSensor: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END getUnbockedUsableSectionsContainingSensor;

      -- Get the two sections surrounding the sensor that
      --   are either occupied or reserved
      PROCEDURE GetOccResSections (
            SensorID      :        Positive;
            FirstSection  :    OUT sectionNodePtr;
            SecondSection :    OUT sectionNodePtr;
            searchOutcome :    out natural) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         FirstSection := NULL;
         SecondSection := NULL;
         
         -- Try to find two sections that are occupied/reserved and which contain the sensor
         WHILE SectionPtr /= NULL LOOP
            IF   (SectionPtr.SensorList.head.id = SensorId OR
                  SectionPtr.SensorList.tail.id = SensorId) 
            AND  (sectionPtr.state = Occupied OR sectionPtr.state = Reserved) 
            THEN
               IF FirstSection = NULL THEN
                  FirstSection := sectionPtr;
               else
                  secondSection := sectionPtr;
                  exit;
               end if;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         
         if firstSection = null and secondSection = null then
            searchOutcome := 1;
         elsif secondSection = null then
            searchOutcome := 2;
         elsif firstSection.trainId /= secondSection.trainId then
            searchOutcome := 3;
         elsif firstSection.state = occupied and secondSection.state = occupied then
            searchOutcome := 4;
         else
            searchOutcome := 5;   -- what if both sections are reserved? not going to happen
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetOccResSections: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END GetOccResSections;

      -- Remove the last sensor in a train's sensor list
      PROCEDURE RemoveLastSensor (
            TrainId : TrainIdType) IS
         TrainPtr  : trainNodePtr   := TrainList;
         SensorPtr : SensorNodePtr;
         sPtr      : sensorNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               TrainPtr.SensorCount := TrainPtr.SensorCount - 1;
               SensorPtr := TrainPtr.SensorList.Head;
               WHILE SensorPtr /= NULL LOOP
                  IF SensorPtr.Next = TrainPtr.SensorList.Tail THEN
                     myPutLine("      <<<<<<<<<<< " & integer'image(sensorPtr.next.id) & " sensor removed from tail" );
                     sPtr := sensorPtr.next;
                     SensorPtr.Next := NULL; 
                     disposeSensorNode(sPtr);
                     TrainPtr.SensorList.Tail := SensorPtr;
                     RETURN;
                  END IF;
                  SensorPtr := SensorPtr.Next;
               END LOOP;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in RemoveLastSensor: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      END RemoveLastSensor;

      function sensorUnderTrain(trainId : trainIdType; sensor : positive) return boolean is
         sensorsPtr : sensorArrayAccess;
         last       : positive;
      begin
         sensorsPtr := GetSensors(TrainId);
         last := sensorsPtr'last;
         if sensorsPtr(1) = sensor or sensorsPtr(last) = sensor then
            disposeSensorArray(sensorsPtr);
            return false;
         end if;
         for i in 2..last-1 loop
            if sensor = sensorsPtr(i) then
               disposeSensorArray(sensorsPtr);
               return true;
            end if;
         end loop;
         disposeSensorArray(sensorsPtr);
         return false;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in sensorUnderTrain: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end sensorUnderTrain;
      
      function sensorIsNextToLast(trainId : trainIdType; sensor : positive) return boolean is
         sensorsPtr : sensorArrayAccess;
         last       : positive;
      begin
         sensorsPtr := GetSensors(TrainId);
         last := sensorsPtr'last;
         if sensorsPtr(last-1) = sensor then
            disposeSensorArray(sensorsPtr);
            return true;
         else
            disposeSensorArray(sensorsPtr);
            return false;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in sensorIsNextToLast: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end sensorIsNextToLast;
      

      -- Get the sensor at the front of a train's reserved section ptr
      function  getSensorAtFrontOfReservedSectionPtr(section : sectionNode) return sensorNodePtr is
         trainId   : trainIdType;
         ptr       : sensorNodePtr;      -- train's front sensor ptr
      BEGIN
         trainId := section.trainId;
         ptr := getFrontSensorPtr(trainId);
         if ptr = section.sensorList.head then
            return section.sensorList.tail;
         else   
            return section.sensorList.head;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getSensorAtFrontOfReservedSectionPtr: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end getSensorAtFrontOfReservedSectionPtr;

      -- Get the sensor at the front of a train's reserved section
      function  getSensorAtFrontOfReservedSection(section : sectionNode) return positive is
         trainId   : trainIdType;
         s1        : positive;      -- train's front sensor
      BEGIN
         trainId := section.trainId;
         s1 := getFrontSensor(trainId);
         if s1 = section.sensorList.head.id then
            return section.sensorList.tail.id;
         else   
            return section.sensorList.head.id;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getSensorAtFrontOfReservedSection: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end getSensorAtFrontOfReservedSection;

      -- Get a train's front sensor ptr
      function GetFrontSensorPtr(TrainId : TrainIdType) return sensorNodePtr is
         TrainPtr : trainNodePtr := TrainList;
         ptr      : sensorNodePtr := null;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               ptr := TrainPtr.SensorList.head;
               exit;                        
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return ptr;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetFrontSensorPtr: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end GetFrontSensorPtr;
      
      -- Get a train's back sensor ptr
      function GetBackSensorPtr(TrainId : TrainIdType) return sensorNodePtr is
         TrainPtr : trainNodePtr := TrainList;
         ptr      : sensorNodePtr := null;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               ptr := TrainPtr.SensorList.tail;
               exit;                        
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return ptr;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetBackSensorPtr: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end GetBackSensorPtr;
      
      -- Get a train's front sensor
      function GetFrontSensor(TrainId : TrainIdType) return Positive is
         TrainPtr : trainNodePtr := TrainList;
         frontId   : positive;
      BEGIN
         frontId := Positive'Last;
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               frontId := TrainPtr.SensorList.head.id;
               return  frontId;                        
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return frontId;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetFrontSensor: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end GetFrontSensor;

      -- Get a train's back sensor
      function GetBackSensor(TrainId : TrainIdType) return Positive is
         TrainPtr : trainNodePtr := TrainList;
         backId   : positive;
      BEGIN
         BackId := Positive'Last;
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               BackId := TrainPtr.SensorList.tail.id;
               return  backId;                        
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
         return backId;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetBackSensor: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end GetBackSensor;

      PROCEDURE GetBackSensor (TrainId : TrainIdType; BackId  : OUT Positive) IS
         TrainPtr : trainNodePtr := TrainList;
      BEGIN
         BackId := Positive'Last;
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               BackId := TrainPtr.SensorList.tail.id;
               return;                        -- mo 1/31/12
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetBackSensor: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      END GetBackSensor;
      
      procedure flipSensor(sensorPtr : sensorNodePtr) is 
      begin   
         -- sensorPtr.StartTime := Clock;
         if sensorPtr.state = open then
            sensorPtr.state := closed;
         else
            sensorPtr.state := open;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in flipSensor: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(SensorPtr.id));
            RAISE;
      end flipSensor;

      procedure errorStopTrainsAtBadSensor(idSensorCase         : positive;
                                           sx                   : positive;
                                           leftSectionPtr       : sectionNodePtr;
                                           rightSectionPtr      : sectionNodePtr) is
         train1, train2         : natural;
      begin
         myPutLine("      -------------IdentifyTrainV3: EXPECTATION ERROR at sensor " & 
                   integer'image(sx) & 
                   " case #" & 
                   integer'image(idSensorCase));
         train1 := 0;
         train2 := 0;
         if leftSectionPtr /= null then
            train1 := leftSectionPtr.trainId;
         end if;
         if rightSectionPtr /= null then
            train2 := rightSectionPtr.trainId;
         end if;
         if train1 /= 0 then
            myPutLine("                                    stopping train " & integer'image(train1));
            sendToTrainQueue(makeSensorErrorMsg(sx), train1);
         end if;
         if train2 /= 0 and train2 /= train1 then
            myPutLine("                                    stopping train " & integer'image(train2));
            sendToTrainQueue(makeSensorErrorMsg(sx), train2);
         end if;
      end errorStopTrainsAtBadSensor;

      function trainInSection(trainId : trainIdtype; sp : sectionNodePtr) return boolean is
      begin
         if trainId = 0 then
            return false;
         end if;
         if sp /= null and then sp.trainId = trainId then
            return true;
         else
            return false;
         end if;
      end trainInSection;

      function sectionReserved(sp : sectionNodePtr) return boolean is
      begin
         if sp /= null and then sp.state = reserved then
            return true;
         else
            return false;
         end if;
      end sectionReserved;
      
      function sectionOccupied(sp : sectionNodePtr) return boolean is
      begin
         if sp /= null and then sp.state = occupied then
            return true;
         else
            return false;
         end if;
      end sectionOccupied;


      procedure identifySensor(sx               : positive;               -- MO March 2014
                               idSensorCase     : out positive;
                               expectationError : out boolean;
                               trainId          : out trainIdType;
                               leftSectionPtr   : out sectionNodePtr;
                               rightSectionPtr  : out sectionNodePtr) is
         sensorPtr                  : sensorNodePtr;
         trainPtr                   : trainNodePtr;
         sId                        : positive;
         section1Ptr, section2Ptr   : sectionNodePtr;
         searchOutcome              : natural;
      begin
         expectationError := false;
         leftSectionPtr := null;
         rightSectionPtr := null;
         trainId := 0;
         
         -- Case 1
         -- see if the sensor is legal
         getSensor(sx, sensorPtr);
         if sensorPtr = null then
            idSensorCase := 1;
            return;
         end if;
         
         getOccResSections(sx, section1Ptr, section2Ptr, searchOutcome);
         
         -- Case 2
         -- for each train in the train list 
         --     check if sx equals the back sensor
         trainPtr := trainList;
         while trainPtr /= null loop
            sId := getBackSensor(trainPtr.trainId);
            if sx = sId then
               idSensorCase := 2;
               trainId := trainPtr.trainId;
               -- Check the expectation 
               if sectionOccupied(section1Ptr) and trainInSection(trainId, section1Ptr) then
                  leftSectionPtr := section1Ptr;
                  rightSectionPtr := section2Ptr;
               else
                  leftSectionPtr := section2Ptr;
                  rightSectionPtr := section1Ptr;
               end if;
               if trainInSection(trainId, rightSectionPtr) then
                  expectationError := true;
               end if;
               -------------------------   
               return;
            end if;
            trainPtr := trainPtr.next;
         end loop;
         
         -- Case 3
         -- for each train in the train list
         --     check if sx is under the train
         trainPtr := trainList;
         while trainPtr /= null loop
            if sensorUnderTrain(trainPtr.trainId, sx) then
               idSensorCase := 3;
               trainId := trainPtr.trainId;
               -- Check the expectation
               if sectionOccupied(section1Ptr) and sectionOccupied(section2Ptr) and
                  trainInSection(trainId, section1Ptr) and trainInSection(trainId, section2Ptr) then
                  leftSectionPtr := section1Ptr;    -- actually we don't care about left and right
                  rightSectionPtr := section2Ptr;
               else
                  expectationError := true;
               end if;
               ------------------------
               return;
            end if;
            trainPtr := trainPtr.next;
         end loop;
         
         -- Case 4
         -- for each train in the train list
         --     check if sx is equal to the front sensor
         trainPtr := trainList;
         while trainPtr /= null loop
            sId := getFrontSensor(trainPtr.trainId);
            if sx = sId then
               idSensorCase := 4;
               trainId := trainPtr.trainId;
               -- Check the expectation
               if not (trainInSection(trainId, section1Ptr) and trainInSection(trainId, section2Ptr)) then
                  expectationError := true;
               elsif sectionReserved(section1Ptr) and sectionOccupied(section2Ptr) then
                  leftSectionPtr := section1Ptr;
                  rightSectionPtr := section2Ptr;
               elsif sectionReserved(section2Ptr) and sectionOccupied(section1Ptr) then
                  leftSectionPtr := section2Ptr;
                  rightSectionPtr := section1Ptr;
               else
                  expectationError := true;
               end if;
               ------------------------
               return;
            end if;
            trainPtr := trainPtr.next;
         end loop;
         
         -- Case 5
         -- If at this point, we have a (reserved,not reserved) pair 
         -- this should establish case 5
         idSensorCase := 5;
         if sectionReserved(section1Ptr) and not sectionReserved(section2Ptr) then
            leftSectionPtr := section2Ptr;
            rightSectionPtr := section1Ptr;
            trainId := section1Ptr.trainId;
            if trainInSection(trainId, leftSectionPtr) then
               expectationError := true;
            end if;
            return;
         elsif sectionReserved(section2Ptr) and not sectionReserved(section1Ptr) then
            trainId := section2Ptr.trainId;
            leftSectionPtr := section1Ptr;
            rightSectionPtr := section2Ptr;
            if trainInSection(trainId, leftSectionPtr) then
               expectationError := true;
            end if;
            return;
         end if;

         -- Case 6
         -- This encompasses the case in which sx cannot be associated with any train and trainId is 0.
         idSensorCase := 6;
         return;
               
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in identifySensor: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(sx));
            RAISE;
      end identifySensor;
      

      -- Send a message to a specific TrainQueue
      PROCEDURE SendToTrainQueue (
            Cmd : MessageType;
            Id  : Positive) IS
         TrainPtr : trainNodePtr := TrainList;
      BEGIN
         myPutLine("      " & toEnglish(cmd) & "       LayoutPkg to train queue");
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = Id THEN
               TrainPtr.Queue.putMessage(Cmd);
               RETURN;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SendToTrainQueue: " & Exception_Information(Error));
            RAISE;
      END SendToTrainQueue;

      -- Send a message to every TrainQueue
      PROCEDURE SendToAllTrainQueues (
            Cmd : MessageType) IS
         TrainPtr : trainNodePtr := TrainList;
      BEGIN
         myPutLine("      " & toEnglish(cmd) & "       LayoutPkg to all train queues");
         WHILE TrainPtr /= NULL LOOP
            TrainPtr.Queue.putMessage(Cmd);
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SendToAllTrainQueue: " & Exception_Information(Error));
            RAISE;
      END SendToAllTrainQueues;

      -- Check if it is possible to reserve this section based on switch states
      FUNCTION IsSectionUseable (SectionPtr : sectionNodePtr) RETURN Boolean IS
         SwitchPtr : SwitchNodePtr := SectionPtr.SwitchList.Head;
         Result    : Boolean;      -- := False;                         -- mo 1/6/12
         SensorPtr : SensorNodePtr;
         Id        : Positive;
      BEGIN
         -- Loop through all switches in this section to see 
         -- if they are all set correctly for usability of this section.
         WHILE SwitchPtr /= NULL LOOP
            -- Assume the section is not usable because this switch is set incorrectly wrong
            result := false;                                             -- mo 1/6/12
            CASE switchPtr.TypeOfSwitch IS
               WHEN Normal =>
                  -- The switch is normal
                  -- Let Id = the section sensor that is at the closed/thrown end of the switch
                  IF SectionPtr.SensorList.head.id = switchPtr.NarrowSensors.head.id THEN
                     Id := SectionPtr.SensorList.tail.id;
                  ELSE
                     Id := SectionPtr.SensorList.head.id;
                  END IF;
                  CASE switchPtr.state IS
                     WHEN Closed =>
                        -- The switch is closed
                        SensorPtr := switchPtr.ClosedSensors.Head;
                        WHILE SensorPtr /= NULL LOOP
                           IF SensorPtr.id = Id THEN
                              -- Sensor Id matches one of the sensors in the switch's closed list
                              -- Therefore from the perspective of this switch the section is usable.
                              Result := True;
                              exit;
                           END IF;
                           SensorPtr := SensorPtr.Next;
                        END LOOP;
                        IF NOT Result THEN
                           -- Sensor Id did not match one of the sensors in the switch's closed list
                           -- so the section is NOT usable
                           RETURN False;
                        END IF;
                     WHEN Thrown =>
                        -- The switch is thrown
                        IF Id /= switchPtr.thrownSensor.id THEN
                           -- Sensor Id did not match the sensor at the switch's thrown end
                           -- so the section is NOT usable
                           RETURN False;
                        END IF;
                     WHEN OTHERS =>
                        -- The switch is moving, so the section is NOT usable
                        RETURN False;
                  END CASE;
               WHEN Crossover =>
                  -- The switch is part of a crossover pair
                  CASE switchPtr.state IS
                     WHEN Closed =>
                        IF NOT (SectionPtr.SensorList.head.id = switchPtr.NarrowSensors.head.id AND
                              SectionPtr.SensorList.tail.id = switchPtr.ClosedSensors.head.id) AND
                              NOT (SectionPtr.SensorList.tail.id = switchPtr.NarrowSensors.head.id AND
                              SectionPtr.SensorList.head.id = switchPtr.ClosedSensors.head.id) AND
                              NOT (SectionPtr.SensorList.head.id = switchPtr.NarrowSensors.tail.id AND
                              SectionPtr.SensorList.tail.id = switchPtr.ClosedSensors.tail.id) AND
                              NOT (SectionPtr.SensorList.tail.id = switchPtr.NarrowSensors.tail.id AND
                              SectionPtr.SensorList.head.id = switchPtr.ClosedSensors.tail.id) THEN
                           RETURN False;
                        END IF;
                     WHEN Thrown =>
                        IF NOT (SectionPtr.SensorList.head.id = switchPtr.NarrowSensors.head.id AND
                              SectionPtr.SensorList.tail.id = switchPtr.NarrowSensors.tail.id) AND
                              NOT (SectionPtr.SensorList.tail.id = switchPtr.NarrowSensors.head.id AND
                              SectionPtr.SensorList.head.id = switchPtr.NarrowSensors.tail.id) THEN
                           RETURN False;
                        END IF;
                     WHEN OTHERS =>
                        RETURN False;
                  END CASE;
            END CASE;
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
         RETURN True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IsSectionUseable: " & Exception_Information(Error));
            raise;
      END IsSectionUseable;

      FUNCTION AllFree (
            SectList : sectionNodeList)
        RETURN Boolean IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         IF SectionPtr = NULL THEN
            RETURN False;
         END IF;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state /= Free OR NOT IsSectionUseable(sectionPtr) THEN
               RETURN False;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         RETURN True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AllFree: " & Exception_Information(Error));
            raise;
      END AllFree;

      -- Get the section based on the sensors
      PROCEDURE FindSection (
            FirstId    :        Positive;
            SecondId   :        Positive;
            SectionPtr :    OUT SectionNodePtr) IS
      BEGIN
         SectionPtr := SectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.SensorList.head.id = FirstId
                  AND SectionPtr.SensorList.tail.id = SecondId THEN
               RETURN;
            ELSIF SectionPtr.SensorList.head.id = SecondId
                  AND SectionPtr.SensorList.tail.id = FirstId THEN
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindSection: " & Exception_Information(Error));
            put_line("    firstId" & integer'image(firstId) & " secondId" & integer'image(secondId));
            raise;
      END FindSection;

      -- Get the sections based on the list of sensors
      PROCEDURE FindAllSections (
            OutSectList :    OUT sectionNodeList;
            Sensors     :        SensorArrayType) IS
         SectionPtr : SectionNodePtr;
         FirstId    : Positive;
         SecondId   : Positive;
      BEGIN
         FirstId := Sensors(1);
         FOR I IN Sensors'First+1..Sensors'Last LOOP
            SecondId := Sensors(I);
            FindSection(FirstId, SecondId, SectionPtr);
            IF SectionPtr /= NULL THEN
               AddToEnd(OutSectList, SectionPtr);
            END IF;
            FirstId := SecondId;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindAllSections: " & Exception_Information(Error));
            raise;
      END FindAllSections;

      -- Place the train
      PROCEDURE PlaceTrainInSections (
            SectList : sectionNodeList;
            TrainId  : TrainIdType) IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            sectionPtr.state := Occupied;
            sectionPtr.trainId := TrainId;
            SendToOutQueue(makePutSectionStateMsg(SectionPtr.Id, Occupied));
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in PlaceTrainInSections: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END PlaceTrainInSections;

      --x can rewrite this with a for loop once I have blockingSection.sectionPtr
      PROCEDURE BlockSections (BlockingList : blockingSectionList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         IF BlockingList.Head /= NULL THEN
            WHILE SectionPtr /= NULL LOOP
               IF IsIn(BlockingList, SectionPtr.Id) THEN
                  sectionPtr.state := Blocked;
                  SectionPtr.BlockCount := SectionPtr.BlockCount + 1;
                  SendToOutQueue(makePutSectionStateMsg(SectionPtr.Id, Blocked));
               END IF;
               SectionPtr := SectionPtr.Next;
            END LOOP;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line(" **************** EXCEPTION Layout pkg in BlockSections:" & Exception_Information(Error));
            raise;
      END BlockSections;

      PROCEDURE GetSensor(
            SensorId :        Positive;
            Sensor   :    OUT sensorNodePtr) IS
         SensorPtr : SensorNodePtr := SensorList.Head;
      BEGIN
         sensor := null;
         WHILE SensorPtr /= NULL LOOP
            IF SensorPtr.id = SensorId THEN
               Sensor := sensorPtr;
               RETURN;
            END IF;
            SensorPtr := SensorPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetSensor: " & Exception_Information(Error));
            put_line("    sensorId" & integer'image(sensorId));
            raise;
      END GetSensor;

      -- mo 1/18/12
      function isNewTrain(TrainId : TrainIdType) return boolean is
         TrainPtr  : trainNodePtr := trainList;
      begin
         while trainPtr /= null loop
            if trainPtr.trainId = trainId then
               return false;
            end if;
            trainPtr := trainPtr.next;
         end loop;
         return true;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in isNewTrain: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      end isNewTrain;
  
      
      FUNCTION IsIn (
            Sections : sectionNodeList;
            Id       : Positive)
        RETURN Boolean IS
         SectionPtr : SectionNodePtr := Sections.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Id = Id THEN
               RETURN True;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         RETURN False;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IsIn: " & Exception_Information(Error));
            put_line("    Id" & integer'image(Id));
            raise;
      END IsIn;


      PROCEDURE GetFreeSection (
            SectList   :        sectionNodeList;
            OutSectPtr :    OUT sectionNodePtr) IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Free 
            AND IsSectionUseable(sectionPtr) THEN
               OutSectPtr := sectionPtr;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         OutSectPtr := NULL;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetFreeSection: " & Exception_Information(Error));
            raise;
      END GetFreeSection;

      PROCEDURE GetTrainSensorList (
            TrainId :        TrainIdType;
            Sensors :    OUT SensorNodeList) IS
         TrainPtr : trainNodePtr := TrainList;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               Sensors := TrainPtr.SensorList;
               RETURN;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetTrainSensorList: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END GetTrainSensorList;


      -- PROCEDURE SetNotUseable (
            -- SwitchPtr : SwitchNodePtr) IS
         -- ThrownSectionList : sectionNodeList;
         -- ClosedSectionList : sectionNodeList;
         -- SectionPtr        : SectionNodePtr;
      -- BEGIN
         -- GetSections(switchPtr, ThrownSectionList, ClosedSectionList);
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- SectionPtr.IsUseable := False;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- SectionPtr.IsUseable := False;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
      -- EXCEPTION
         -- WHEN Error : OTHERS =>
            -- put_line("**************** EXCEPTION Layout pkg in SetNotUseable: " & Exception_Information(Error));
            -- raise;
      -- END SetNotUseable;

      PROCEDURE GetSection (
            SectionPtr    :    OUT SectionNodePtr;
            FrontSensorId :        Positive;
            BackSensorId  :        Positive) IS
      BEGIN
         SectionPtr := SectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF (SectionPtr.SensorList.head.id = FrontSensorId AND
                  SectionPtr.SensorList.tail.id = BackSensorId) OR
                  (SectionPtr.SensorList.head.id = BackSensorId AND
                  SectionPtr.SensorList.tail.id = FrontSensorId) THEN
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetSection: " & Exception_Information(Error));
            put_line("    frontSensorId" & integer'image(frontSensorId) & " backSensorId" & integer'image(backSensorId));
            raise;
      END GetSection;

      -- Is it possible for a train to move this switch
      PROCEDURE MoveSwitchPossible (
            SwitchPtr :        SwitchNodePtr;
            TrainId   :        TrainIdType;
            Result    :    OUT Boolean) IS
         ThrownSectionList : sectionNodeList;
         ClosedSectionList : sectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         GetSections(switchPtr, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Occupied OR
                  (sectionPtr.state = Reserved AND
                  sectionPtr.trainId /= TrainId) THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Occupied OR
                  (sectionPtr.state = Reserved AND
                  sectionPtr.trainId /= TrainId) THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         Result := True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveSwitchPossible: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END MoveSwitchPossible;

      -- Is it possible to move this switch
      PROCEDURE MoveSwitchPossible (
            SwitchPtr :        SwitchNodePtr;
            Result    :    OUT Boolean) IS
         ThrownSectionList : sectionNodeList;
         ClosedSectionList : sectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         GetSections(switchPtr, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Occupied THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Occupied THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         Result := True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveSwitchPossible 2: " & Exception_Information(Error));
            raise;
      END MoveSwitchPossible;

      PROCEDURE FindIdOfTrainLoosingReservation (SwitchPtr : SwitchNodePtr; 
                                                 trainId : out TrainIdType; 
                                                 thereIsReservation : out boolean) IS
         ThrownSectionList : sectionNodeList;
         ClosedSectionList : sectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         thereIsReservation := false;
         trainId := trainIdType'first;
         GetSections(switchPtr, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Reserved THEN
               thereIsReservation := true;
               trainId := sectionPtr.trainId;
               return;
               END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Reserved THEN
               thereIsReservation := true;
               trainId := sectionPtr.trainId;
               return;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindIdOfTrainLoosingReservation: " & Exception_Information(Error));
            raise;
      END FindIdOfTrainLoosingReservation;

      PROCEDURE SendLoseReservationMessages (SwitchPtr : SwitchNodePtr) IS
         ThrownSectionList : sectionNodeList;
         ClosedSectionList : sectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         GetSections(switchPtr, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Reserved THEN
               SendToTrainQueue(makeLoseReservationMsg(sectionPtr.trainId), sectionPtr.trainId);
               return;
               END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF sectionPtr.state = Reserved THEN
               SendToTrainQueue(makeLoseReservationMsg(sectionPtr.trainId), sectionPtr.trainId);
               return;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SendLoseReservationMessage: " & Exception_Information(Error));
            raise;
      END SendLoseReservationMessages;


      procedure disposeSensorNode(ptr : in out sensorNodePtr) is
      begin
         if ptr /= null then
            disposeBasicSensorNode(ptr);
         end if;
      end disposeSensorNode;
               
      procedure makeEmptySensorNodeList(sol : in out SensorNodeList) is 
         cur, temp  : sensorNodePtr;
      begin
         cur := sol.head;
         while cur /= null loop
            temp := cur;
            cur := cur.next;
            disposeSensorNode(temp);
         end loop;
      end makeEmptySensorNodeList;
      
      -- procedure disposetrainNode(ptr : in out trainNodePtr) is
      -- begin
         -- if ptr /= null then
            -- makeEmptySensorNodeList(ptr.sensorList);
            -- disposeBasictrainNode(ptr);
         -- end if;
      -- end disposetrainNode;

      procedure updateTrainSensors(TrainId : TrainIdType; Sensors : SensorArrayType) IS
      -- pre TrainId is in TrainList
         TrainPtr  : trainNodePtr := trainList;
         SensorPtr : sensorNodePtr;
      begin
      
         -- Find the train
         while trainPtr.trainId /= trainId loop
            trainPtr := trainPtr.next;
         end loop;
         
         -- Remove old sensors
         makeEmptySensorNodeList(trainPtr.sensorList);              
         
         -- Add the new sensors   
         TrainPtr.SensorCount := Sensors'Length;
         TrainPtr.SensorList.Head := NEW SensorNode;    
         TrainPtr.SensorList.Tail := TrainPtr.SensorList.Head;
         FOR I IN Sensors'RANGE LOOP
            GetSensor(Sensors(I), SensorPtr);
            TrainPtr.SensorList.tail := SensorPtr;
            IF I /= Sensors'Last THEN
               TrainPtr.SensorList.Tail.Next := NEW SensorNode;
               TrainPtr.SensorList.Tail := TrainPtr.SensorList.Tail.Next;
            END IF;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in updateTrainSensors: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
       end updateTrainSensors;
    
      PROCEDURE AddNewTrain (TrainId : TrainIdType; Sensors : SensorArrayType) IS
         TrainPtr  : trainNodePtr;
         SensorPtr : sensorNodePtr;
      BEGIN
         IF TrainList = NULL THEN
            TrainList := NEW trainNode;
            TrainList.TrainId := TrainId;
            CommandQueueManager.TrainIdQueueList.GetQueue(TrainId, TrainList.Queue);
            TrainPtr := TrainList;
         ELSE
            TrainPtr := TrainList;
            WHILE TrainPtr.Next /= NULL LOOP
               TrainPtr := TrainPtr.Next;
            END LOOP;
            TrainPtr.Next := NEW trainNode;
            TrainPtr.Next.TrainId := TrainId;
            CommandQueueManager.TrainIdQueueList.GetQueue(TrainId, TrainPtr.Next.Queue);
            TrainPtr := TrainPtr.Next;
         END IF;

         TrainPtr.SensorCount := Sensors'Length;
         TrainPtr.SensorList.Head := NEW SensorNode;
         TrainPtr.SensorList.Tail := TrainPtr.SensorList.Head;
         FOR I IN Sensors'RANGE LOOP
            GetSensor(Sensors(I), SensorPtr);
            TrainPtr.SensorList.tail := SensorPtr;
            IF I /= Sensors'Last THEN
               TrainPtr.SensorList.Tail.Next := NEW SensorNode;
               TrainPtr.SensorList.Tail := TrainPtr.SensorList.Tail.Next;
            END IF;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddNewTrain: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END AddNewTrain;
   
-------------------- End helper function   -------------------------  
--------------------------- 2f -------------------------------------
--------------------------- 2f -------------------------------------
--------------------------- 2f -------------------------------------

   END LayoutManager;
   
-------------------- End LayoutManager ---------------------------
--------------------------- 2 ------------------------------------
--------------------------- 2 ------------------------------------
--------------------------- 2 ------------------------------------



-------------------------- 3 -------------------------------------
-------------------------- 3 -------------------------------------
-------------------------- 3 -------------------------------------
-------------------- Begin LayoutTaskType ------------------------

   -- Give the XMLParser a pointer to the LayoutManager
   -- Open the XML file
   -- Have XMLParser parse the file and give the information
   --   to LayoutManager to create data structures
   -- Send PutReadLayoutResponse
   FUNCTION ParseXML (LayoutPtr : LayoutManagerAccess) RETURN Boolean IS
      My_Reader : XMLParser.Reader;
      Input     : File_Input;
   BEGIN
      XMLParser.SetLayout(LayoutPtr);
      Open(To_String(LayoutPtr.GetXMLFilename), Input);
      Parse(My_Reader, Input);
      Close(Input);
      SendToOutQueue(makePutReadLayoutResponseMsg(1, 0));
      RETURN True;
   EXCEPTION
      WHEN Error : Name_Error =>
         put_line("**************** EXCEPTION Layout pkg in ParseXML: Bad XML filename: " & To_String(LayoutPtr.GetXMLFilename));
         SendToOutQueue(makePutReadLayoutResponseMsg(2, 1));
         raise;
      WHEN Error : OTHERS =>
         put_line("**************** EXCEPTION Layout pkg in ParseXML: Error parsing XML File:" & Exception_Information(Error));
         SendToOutQueue(makePutReadLayoutResponseMsg(2, 2));
         raise;
   END ParseXML;

   TASK BODY LayoutTaskType IS
      LayoutPtr : LayoutManagerAccess;
      Cmd       : MessageType;
   BEGIN
      ACCEPT SetLayout (
            L : IN     LayoutManagerAccess) DO
         LayoutPtr := L;
      END;

      LOOP
         BEGIN
               CommandQueueManager.LayoutQueue.GetMessage(Cmd);
               myPutLine("      " & toEnglish(cmd) & "       received by LayoutTask");
               CASE Cmd.ByteArray(1) IS
                  WHEN OPC_INPUT_REP =>
                     -- Sensor fired
                     DECLARE
                        SensorId  : Positive;
                        isHigh    : Boolean;
                     BEGIN
                        splitInputRepMsg(Cmd, SensorId, isHigh);
                        if isHigh then          -- mo 1/16/12
                           myPutLine("       ignoring high sensor " & integer'image(sensorId));
                        else                        
                           -- LayoutPtr.IdentifyTrainV1(SensorId);                                     
                           -- LayoutPtr.IdentifyTrainV2(SensorId);                                    
                           LayoutPtr.IdentifyTrainV3(SensorId);                                      
                        end if;
                     END;
                  WHEN OPC_SW_REQ =>
                     -- Request to move a switch
                     DECLARE
                        SwitchId : Positive;
                        State    : SwitchStateType;
                     BEGIN
                        SplitSwReqMsg(Cmd, SwitchId, State);
                        LayoutPtr.MoveSwitch(SwitchId, State);
                     END;
                  WHEN OPC_SW_REP =>
                     -- A switch finished moving
                     DECLARE
                        SwitchId : Positive;
                        State    : SwitchStateType;
                     BEGIN
                        SplitSwRepMsg(Cmd, SwitchId, State);
                        LayoutPtr.SwitchFinishedMoving(SwitchId, State);
                     END;
                  WHEN UZero =>
                     -- Extended Messages
                     IF Cmd.Size >= 2 THEN
                        CASE Cmd.ByteArray(2) IS
                           WHEN DoReadLayout =>
                              DECLARE
                                 Result      : Boolean          := False;
                                 XMLFilename : Unbounded_String;
                              BEGIN
                                 SplitDoReadLayoutMsg(Cmd, XMLFilename);
                                 LayoutPtr.SetXMLFilename(XMLFilename);
                                 Result := ParseXML(LayoutPtr);
                              END;
                           WHEN GetSwitchStates =>
                              LayoutPtr.GetSwitchStates;
                           when doMakeSectionUseable =>
                              declare
                                 sensor1, sensor2 : positive;
                              begin
                                 splitDoMakeSectionUseableMsg(cmd, sensor1, sensor2);
                                 LayoutPtr.makeSectionUseable(sensor1, sensor2);
                              end;
                           WHEN OTHERS =>
                              NULL;
                        END CASE;
                     END IF;
                  WHEN 16#ff# =>
                     EXIT; -- for testing
                  WHEN OTHERS =>
                     NULL;
               END CASE;
            -- END IF;                                            
         EXCEPTION
            WHEN Error : OTHERS =>
               put_line("**************** EXCEPTION in LayoutTask: " & Exception_Information(Error));
         END;
      END LOOP;
   END LayoutTaskType;
   

-------------------- End LayoutTaskType --------------------------
-------------------------- 3 -------------------------------------   
-------------------------- 3 -------------------------------------   
-------------------------- 3 -------------------------------------   
END LayoutPkg;
