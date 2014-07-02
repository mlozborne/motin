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
                  IF PrevSectionPtr.Section.SensorList.Head.Sensor.Id /= SecondSensor AND
                        NOT IsIn(PrevSectionPtr.Section.NextSectionList, ThisSectionPtr.Section.Id) AND
                        PrevSectionPtr.Section.SensorList.Tail.Sensor.Id /= SecondSensor AND
                        NOT IsIn(PrevSectionPtr.Section.PrevSectionList, ThisSectionPtr.Section.Id) THEN
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
         TrainPtr      : TrainNodePtr   := pkgTrainList;
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
                  --vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
                  --x this code seems pointless and creates a memory leak
                  SensorPtr := TrainPtr.SensorList.Head;
                  WHILE SensorPtr /= NULL LOOP
                     mySensors(I) := sensorPtr.sensor.id;
                     I := I + 1;
                     SensorPtr := SensorPtr.Next;
                  END LOOP;
                  --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
         sectionPtr := pkgSectionList.head;              
         while sectionPtr /= null loop
            if sectionPtr.section.trainId = trainId and (sectionPtr.section.state = occupied or sectionPtr.section.state = reserved) then
               SectionPtr.Section.State := Free;
               SectionPtr.Section.trainId := 0;
               ReleaseBlockings(SectionPtr.Section.BlockingList);
               SendToOutQueue(makePutSectionStateMsg(SectionPtr.Section.Id, Free));
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
         SwitchPtr : SwitchNodePtr := pkgSwitchList.Head;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, SwitchPtr.Switch.State));
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
         s1                  : sensorObjPtr;
         leftSectionPtr     : SectionObjPtr;
         rightSectionPtr    : SectionObjPtr;
         section            : sectionObjPtr;
         sectionNode        : sectionNodePtr;
         oldSensorState     : sensorStateType;
         newSensorState     : sensorStateType;
         trainId             : trainIdType;
         SensorPtrs         : AccessToArrayOfSensorObjPtrType;
         last               : positive;
         idSensorCase       : positive;
      begin
         -- Determine if sensor is in the layout specification even if it exists physical.
         FindSensor(pkgSensorList, sx, SensorPtr);
         IF SensorPtr = NULL THEN
            myPutLine("      -------------IdentifyTrainV3: ERROR(maybe) sensor not recognized " & integer'image(sx) ); 
            return;
         end if;
         
         -- Remember the sensor's state:  oldSensorState
         -- Flip the sensor:  newSensorState
         -- Display the old sensor state
         oldSensorState := sensorPtr.sensor.state;
         flipSensor(sensorPtr);
         newSensorState := sensorPtr.sensor.state;
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
                  section := sectionNode.section;
                  section.state := free;
                  SendToOutQueue(makePutSectionStateMsg(section.Id, Free));
                  ReleaseBlockings(section.BlockingList);
                  section.trainId := 0;
                  RemoveLastSensor(TrainId);
                  SendToTrainQueue(makeBackSensorFiredMsg(TrainId), TrainId);
               end loop;   
               putTrainPositionMsg(TrainId);
               SendToAllTrainQueues(makeTryToMoveAgainMsg);               
               disposeArrayOfSensorObjPtr(sensorPtrs);            
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
                  IF leftSectionPtr.SensorList.Head.Sensor.Id = sx THEN
                     AddNewSensorToFront(leftSectionPtr.TrainId, leftSectionPtr.SensorList.Tail.Sensor);   
                  ELSE
                     AddNewSensorToFront(leftSectionPtr.TrainId, leftSectionPtr.SensorList.Head.Sensor);
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
                  IF rightSectionPtr.SensorList.Head.Sensor.Id = s1.id THEN                
                     AddNewSensorToFront(rightSectionPtr.TrainId, rightSectionPtr.SensorList.Tail.Sensor);   
                  ELSE
                     AddNewSensorToFront(rightSectionPtr.TrainId, rightSectionPtr.SensorList.Head.Sensor);
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
                     -- if rightSectionPtr.sensorList.head.sensor.id = sf.id then    -- Add sf to front of train
                        -- addNewSensorToFront(trainId, rightSectionPtr.sensorList.head.sensor);  
                     -- else
                        -- addNewSensorToFront(trainId, rightSectionPtr.sensorList.tail.sensor);
                     -- end if;
                     -- if nextFreeSection.sensorList.head.sensor.id = sf.id then   -- Add sf+1 to front of train
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.tail.sensor);
                     -- else
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.head.sensor);
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
         -- sf                   : SensorObjPtr;    -- next sensor in front of train
         -- t1                   : SectionObjPtr;   -- first section containing sx
         -- t2                   : SectionObjPtr;   -- second section containing sx
         -- reservedSection      : sectionObjPtr;
         -- occupiedSection      : sectionObjPtr;
         -- lastSection          : sectionObjPtr;
         -- BackId               : Positive;
         -- oldSensorState       : sensorStateType;
         -- newSensorState       : sensorStateType;
         -- trainId              : trainIdType;
         -- sensorsPtr           : sensorArrayAccess; -- <s1..sn>
      -- BEGIN
          -- myPutLine("      xxxxxxxxxxxxx: In IdentifyTrainV2, sensor = " & integer'image(sensorId)); 
          
         -- -- Get a pointer to the sensor object.
         -- -- If not found then ignore this sensor and return
         -- FindSensor(pkgSensorList, SensorID, sx);         
         -- IF sx = NULL THEN
            -- myPutLine("      xxxxxxxxxxxxx: MAYBE ERROR sensor not recognized " & integer'image(sensorId) ); 
            -- return;
         -- end if;
         
         -- -- Keep track of the old and new state of the sensor
         -- oldSensorState := sx.sensor.state;
         -- flipSensor(sx);
         -- newSensorState := sx.sensor.state;
         -- myPutLine("      xxxxxxxxxxxxx: Sensor state = " & sensorStateType'image(oldSensorState)); 
         
         -- -- Inform Othrottles that sensor has fired and its new state                 -- mo 1/30/12
         -- SendToOutQueue(makePutSensorStateMsg(SensorId, newSensorState));
         
         -- -- Find the two sections that contain the sensor
         -- getUnblockedUsableSectionsContainingSensor(sx.Sensor.Id, t1, t2);
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
            -- if reservedSection.sensorList.head.sensor.id = sensorId then
               -- sf := reservedSection.sensorList.tail.sensor;
            -- else   
               -- sf := reservedSection.sensorList.head.sensor;
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
            -- IF t1.sensorList.head.sensor.id = backId OR t1.SensorList.Tail.sensor.id = backId  THEN
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
            sensorsPtr   : sensorArrayAccess;
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
         section1           : SectionObjPtr;
         section2           : SectionObjPtr;
         section            : sectionObjPtr;
         sectionNode        : sectionNodePtr;
         s1, sn             : sensorObjPtr;
         -- sf front of reserved section, s1 front of train, sn back of train
         oldSensorState     : sensorStateType;
         newSensorState     : sensorStateType;
         trainId             : trainIdType;
         searchOutcome      : natural;
         SensorPtrs         : AccessToArrayOfSensorObjPtrType;
         last               : positive;
      BEGIN
         FindSensor(pkgSensorList, SensorID, SensorPtr);
         
         IF SensorPtr = NULL THEN
            myPutLine("      -------------IdentifyTrainV1: ERROR(maybe) sensor not recognized " & integer'image(sensorId) ); 
            return;
         end if;
         
         oldSensorState := sensorPtr.sensor.state;
         flipSensor(sensorPtr);
         newSensorState := sensorPtr.sensor.state;
         if oldSensorState = open then
            myPutLine("      -------------IdentifyTrainV1: sensor " & integer'image(sensorId) & " open-->closed");   
         else
            myPutLine("      -------------IdentifyTrainV1: sensor " & integer'image(sensorId) & " closed-->open");   
         end if;
         
         -- Inform Othrottles that sensor has fired and its new state                 
         SendToOutQueue(makePutSensorStateMsg(SensorId, newSensorState));
         
         -- if not Simulator and Clock - SensorPtr.Sensor.StartTime < 1.0 then
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
                     IF section1.SensorList.Head.Sensor.Id = s1.id THEN                
                        AddNewSensorToFront(section1.TrainId, section1.SensorList.Tail.Sensor);   
                     ELSE
                        AddNewSensorToFront(section1.TrainId, section1.SensorList.Head.Sensor);
                     END IF;
                  ELSE
                     section2.State := Occupied;
                     SendToOutQueue(makePutSectionStateMsg(section2.Id, Occupied));
                     IF section2.SensorList.Head.Sensor.Id = s1.id THEN               
                        AddNewSensorToFront(section2.TrainId, section2.SensorList.Tail.Sensor);
                     ELSE
                        AddNewSensorToFront(section2.TrainId, section2.SensorList.Head.Sensor);
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
                     -- if section1.sensorList.head.sensor.id = sf.id then    -- Add sf to front of train
                        -- addNewSensorToFront(trainId, section1.sensorList.head.sensor);  
                     -- else
                        -- addNewSensorToFront(trainId, section1.sensorList.tail.sensor);
                     -- end if;
                     -- if nextFreeSection.sensorList.head.sensor.id = sf.id then   -- Add sf+1 to front of train
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.tail.sensor);
                     -- else
                        -- addNewSensorToFront(trainId, nextFreeSection.sensorList.head.sensor);
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
                  section := sectionNode.section;
                  section.state := free;
                  SendToOutQueue(makePutSectionStateMsg(section.Id, Free));
                  ReleaseBlockings(section.BlockingList);
                  section.trainId := 0;
                  RemoveLastSensor(TrainId);
                  SendToTrainQueue(makeBackSensorFiredMsg(TrainId), TrainId);
               end loop;   
               putTrainPositionMsg(TrainId);
               SendToAllTrainQueues(makeTryToMoveAgainMsg);               
               disposeArrayOfSensorObjPtr(sensorPtrs);            
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
                  IF section1.SensorList.Head.Sensor.Id = sensorId THEN
                     AddNewSensorToFront(section1.TrainId, section1.SensorList.Tail.Sensor);   
                  ELSE
                     AddNewSensorToFront(section1.TrainId, section1.SensorList.Head.Sensor);
                  END IF;
               ELSE
                  section2.State := Occupied;
                  SendToOutQueue(makePutSectionStateMsg(section2.Id, Occupied));
                  IF section2.SensorList.Head.Sensor.Id = sensorId THEN
                     AddNewSensorToFront(section2.TrainId, section2.SensorList.Tail.Sensor);
                  ELSE
                     AddNewSensorToFront(section2.TrainId, section2.SensorList.Head.Sensor);
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
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
         Sensors    : SensorNodeList;
         OutSectPtr : SectionObjPtr;
      BEGIN
         Result := False;
         GetTrainSensorList(TrainId, Sensors);
         IF Sensors.Head = NULL THEN
            RETURN;
         END IF;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied AND SectionPtr.Section.TrainId = TrainId THEN
               IF SectionPtr.Section.SensorList.Head.Sensor.Id = Sensors.Head.Sensor.Id THEN
                  GetFreeSection(SectionPtr.Section.PrevSectionList, OutSectPtr);
               ELSIF SectionPtr.Section.SensorList.Tail.Sensor.Id = Sensors.Head.Sensor.Id THEN
                  GetFreeSection(SectionPtr.Section.NextSectionList, OutSectPtr);
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
      
      procedure MakeSectionUsable   (sensor1 : positive; sensor2 : positive) is 
         sectionNPtr     :  SectionNodePtr;
         switchStateNPtr : SwitchStateNodePtr;
      begin
         null;
         -- This is a preliminary simplified version
         -- Move all switches in the section to their proper positions and send
         -- a response message back to all OThrottles.
         
         -- Find the section defined by the sensors
         findSection(sensor1, sensor2, sectionNPtr);
                     
         -- Make the section usable by moving the switches to the needed settings
         --x Need to consider if the section is available
         switchStateNPtr := sectionNPtr.section.mySwitchStateList.head;
         while switchStateNPtr /= null loop
            if switchStateNPtr.myState /= switchStateNPtr.switch.state then
               moveSwitch(switchStateNPtr.switch.id, switchStateNPtr.myState);
            end if;
            switchStateNPtr := switchStateNPtr.next;
         end loop;
         
         -- Send a positive response to all OThrottles
         --x Need to consider a negative response
         sendToOutQueue(makePutMakeSectionUsableResponseMsg(sensor1, sensor2, 1));
         
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MakeSectionUsable: " & Exception_Information(Error));
            put_line("    sensor1/sensor2" & integer'image(sensor1) & integer'image(sensor2));
            raise;
      END MakeSectionUsable;      
      
      procedure getPathHelperDF(preSensor      : in positive;
                                fromSensor       : in positive;
                                toSensor         : in positive;
                                sList            : in out naturalListType;
                                sensorsToExclude : in naturalListType;
                                success          : out boolean) is
         thisSection, ptr         : sectionNodePtr;
         sectionList              : sectionNodeList;
         s1, s2, otherSensor      : positive;
      begin
         myPutLine("      pre/from/to " & integer'image(preSensor)
                                        & integer'image(fromSensor)
                                        & integer'image(toSensor));
         success := false;
         -- Find section defined by preSensor/fromSensor
         FindSection(preSensor, fromSensor, thisSection);
         if thisSection = null then
            return;
         end if;
         
         -- Figure out which list to use: nextSectionList or prevSectionList
         s1 := thisSection.section.sensorList.head.sensor.id;
         s2 := thisSection.section.sensorList.tail.sensor.id;
         if fromSensor = s2 then
            sectionList := thisSection.section.nextSectionList;
         else
            sectionList := thisSection.section.prevSectionList;
         end if;

         -- Check each section in section list to see if it contain toSensor
         ptr := sectionList.head;
         while ptr /= null loop
            s1 := ptr.section.SensorList.head.sensor.id;
            s2 := ptr.section.SensorList.tail.sensor.id;
            if toSensor = s1 or toSensor = s2 then
               -- This one contains toSensor: success, add to sensor to list, return
               success := true;
               addFront(sList, toSensor);
               return;
            else
               ptr := ptr.next;
            end if;
         end loop;
         
         -- No section contained toSensor so must recurse
         ptr := sectionList.head;
         while ptr /= null loop
            if ptr.marked then
               ptr := ptr.next;
            else
               ptr.marked := true;
               s1 := ptr.section.SensorList.head.sensor.id;
               s2 := ptr.section.SensorList.tail.sensor.id;
               if fromSensor = s1 then
                  getPathHelperDF(s1, s2, toSensor, sList, sensorsToExclude, success);
                  otherSensor := s2;
              else
                  getPathHelperDF(s2, s1, toSensor, sList, sensorsToExclude, success);
                  otherSensor := s1;
               end if;
               if success then
                  addFront(sList, otherSensor);
                  return;
               else
                  ptr := ptr.next;
               end if;
            end if;
         end loop;
         return;
      end getPathHelperDF;
       
		procedure getTrainPosition(trainId : trainIdType) is
      begin
			putTrainPositionMsg(trainId);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getTrainPosition: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END getTrainPosition;      
      
      
      procedure getPathDF(preSensor : positive; fromSensor : positive; toSensor : positive; sensorsToExclude : naturalListType) is
         sList        : naturalListType;
         success      : boolean := false; 
         ptr1         : sectionNodePtr := pkgSectionList.head;
         ptr2         : sectionNodePtr;
         iter         : naturalListPkg.listIteratorType;
      begin
         -- For all next and previous section lists unmark all section nodes
         while ptr1 /= null loop
            ptr2 := ptr1.section.nextSectionList.head;
            while ptr2 /= null loop
               ptr2.marked := false;
               ptr2 := ptr2.next;
            end loop;
            
            ptr2 := ptr1.section.prevSectionList.head;
            while ptr2 /= null loop
               ptr2.marked := false;
               ptr2 := ptr2.next;
            end loop;
            
            ptr1 := ptr1.next;
         end loop;
         
         -- Call the helper function
         makeEmpty(sList);
         getPathHelperDF(preSensor, fromSensor, toSensor, sList, sensorsToExclude, success);
         if success then
            addFront(sList, fromSensor);
         end if;
         
         -- Display the path
         if getCount(sList) /= 0 then
            iter := moveFront(sList);
            for i in 1..getCount(sList) loop
               myPutLine("        " & integer'image(getCurrent(iter)));
               iter := moveNext(iter);
            end loop;         
         end if;

         -- Send the PutPath message
         sendToOutQueue(makePutPathMsg(sList));
         
         -- Recover list memory
         makeEmpty(sList);
         
         -- makeEmpty(sList);
         -- addEnd(sList, 8);
         -- addEnd(sList, 33);
         -- addEnd(sList, 35);
         -- addEnd(sList, 62);
         -- addEnd(sList, 59);
         -- addEnd(sList, 80);
         -- addEnd(sList, 76);
         -- addEnd(sList, 74);
         -- addEnd(sList, 94);
         -- addEnd(sList, 91);
         -- addEnd(sList, 100);
         -- addEnd(sList, 98);
         -- addEnd(sList, 82);
         -- addEnd(sList, 86);
         -- addEnd(sList, 69);
         -- addEnd(sList, 70);
         -- addEnd(sList, 57);
         -- sendToOutQueue(makePutPathMsg(sList));
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getPathDF: " & Exception_Information(Error));
            put_line("    pre/from/to sensors" & integer'image(preSensor) & 
                                                 integer'image(fromSensor) &
                                                 integer'image(toSensor));
            raise;
      END getPathDF;      
         
      procedure getPathBF(preSensor : positive; fromSensor : positive; toSensor : positive; sensorsToExclude : naturalListType) is
         sList        : naturalListType;
         qu           : sectionNodePtrListType;
         success      : boolean := false; 
         ptr1         : sectionNodePtr := pkgSectionList.head;
         ptr2         : sectionNodePtr;
         ptr          : sectionNodePtr;
         thisSection  : sectionNodePtr;
         iter         : naturalListPkg.listIteratorType;
         s1, s2       : positive;
         firstTime    : boolean;
         listToUse    : sectionNodeList;
   -- Output      : File_Type;
   -- printFlag   : boolean;
      begin
         -- For all next and previous section lists
         --    Unmark and set parent to null in all section nodes
         while ptr1 /= null loop
            ptr2 := ptr1.section.nextSectionList.head;
            while ptr2 /= null loop
               ptr2.marked := false;
               ptr2.parent := null;
               ptr2 := ptr2.next;
            end loop;
            
            ptr2 := ptr1.section.prevSectionList.head;
            while ptr2 /= null loop
               ptr2.marked := false;
               ptr2.parent := null;
               ptr2 := ptr2.next;
            end loop;
            
            ptr1 := ptr1.next;
         end loop;
         
         -- Start breadth first search
         -----------------------------
         -- Add first section to the empty qu
         makeEmpty(qu);
         FindSection(preSensor, fromSensor, thisSection);
         thisSection.pathSensor := fromSensor;
         addEnd(qu, thisSection);
         
         firstTime := true;
         -- printFlag := true;
         while not isEmpty(qu) loop
            -- dequeue
            thisSection := getFront(qu);
            removeFront(qu);

            -- If this what we are looking for success and exit loop
            s1 := thisSection.section.sensorList.head.sensor.id;
            s2 := thisSection.section.sensorList.tail.sensor.id;
            if firstTime then
               firstTime := false;
            elsif s1 = toSensor or s2 = toSensor then
               success := true;
               exit;
            end if;
            
            -- Got to keep going
            -- Consider all sections leaving current section in right direction
            
            -- Figure out which list to use: nextSectionList or prevSectionList
            if thisSection.pathSensor = s2 then
               listToUse := thisSection.section.nextSectionList;
            else
               listToUse := thisSection.section.prevSectionList;
            end if;
            
-- if printFlag then
   -- printFlag := false;
   -- Create (Output, Out_File, "XMLParseTestOutput.txt");
   -- Put_Line(Output, "Section List:");
   -- Print_Sections(GetSectionList, 0, Output, false);
   -- Close(Output);
-- end if;
            
            -- Loop through all the sections in the list.
            -- If a section hasn't been used, mark it and add it to the qu after
            -- determining its parent and pathSensor
            ptr := listToUse.head;
            while ptr /= null loop
               if not ptr.marked then
                  ptr.marked := true;
                  ptr.parent := thisSection;
                  s1 := ptr.section.sensorList.head.sensor.id;
                  s2 := ptr.section.sensorList.tail.sensor.id;
                  if thisSection.pathSensor = s1 then
                     ptr.pathSensor := s2;
                  else
                     ptr.pathSensor := s1;
                  end if;
                  addEnd(qu, ptr);
               end if;
               ptr := ptr.next;
            end loop;
         end loop;
         
         makeEmpty(sList);
         if success then
            -- Build path by traversing parent pointers.
            addFront(sList, toSensor);
            ptr := thisSection.parent;
            while ptr /= null loop
               addFront(sList,ptr.pathSensor);
               ptr := ptr.parent;
            end loop;
         end if;
         
         -- Display the path
         if getCount(sList) /= 0 then
            iter := moveFront(sList);
            for i in 1..getCount(sList) loop
               myPutLine("        " & integer'image(getCurrent(iter)));
               iter := moveNext(iter);
            end loop;         
         end if;

         -- Send the PutPath message
         sendToOutQueue(makePutPathMsg(sList)); 
         
         -- Recover list memory
         makeEmpty(sList);     

         --x recover qu memory
         
         -- makeEmpty(sList);
         -- addEnd(sList, 8);
         -- addEnd(sList, 33);
         -- addEnd(sList, 35);
         -- addEnd(sList, 62);
         -- addEnd(sList, 59);
         -- addEnd(sList, 80);
         -- addEnd(sList, 76);
         -- addEnd(sList, 74);
         -- addEnd(sList, 94);
         -- addEnd(sList, 91);
         -- addEnd(sList, 100);
         -- addEnd(sList, 98);
         -- addEnd(sList, 82);
         -- addEnd(sList, 86);
         -- addEnd(sList, 69);
         -- addEnd(sList, 70);
         -- addEnd(sList, 57);
         -- sendToOutQueue(makePutPathMsg(sList));
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getPathBF: " & Exception_Information(Error));
            put_line("    pre/from/to sensors" & integer'image(preSensor) & 
                                                 integer'image(fromSensor) &
                                                 integer'image(toSensor));
            raise;
      END getPathBF;      
         
      PROCEDURE MoveNextSwitch (
            TrainId : TrainIdType;
            State   : SwitchStateType) IS
         TrainPtr      : TrainNodePtr    := pkgTrainList;
         SectionPtr    : SectionNodePtr;
         FrontSensorId : Positive;
         Result        : Boolean;
         switchPtr     : switchObjPtr;
      BEGIN
         -- Find the train corresponding to TrainId
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               -- Get a pointer to the section at the front of the train
               GetSection(SectionPtr, TrainPtr.SensorList.Head.Sensor.Id, TrainPtr.SensorList.Head.Next.Sensor.Id);
               IF SectionPtr /= NULL THEN   -- I wonder how this could possibly be null??????????
                  FrontSensorId := TrainPtr.SensorList.Head.Sensor.Id;
                  -- Loop forward through the sections until reaching one that has a switch
                  LOOP
                     -- Determine if the next section is in PrevSectionList or the NextSectionList
                     IF SectionPtr.Section.SensorList.Head.Sensor.Id = FrontSensorId THEN
                        SectionPtr := SectionPtr.Section.PrevSectionList.Head;
                     ELSE
                        SectionPtr := SectionPtr.Section.NextSectionList.Head;
                     END IF;
                     -- Exit if the section contains a switch
                     EXIT WHEN SectionPtr.Section.mySwitchStateList.Head /= NULL;      
                     -- Determine the sensor at the front of the section and go round the loop again
                     IF SectionPtr.Section.SensorList.Head.Sensor.Id = FrontSensorId THEN
                        FrontSensorId := SectionPtr.Section.SensorList.Tail.Sensor.Id;
                     ELSE
                        FrontSensorId := SectionPtr.Section.SensorList.Head.Sensor.Id;
                     END IF;
                  END LOOP;
                  
                  -- Get a pointer to the first switch in the section
                  switchPtr := SectionPtr.Section.mySwitchStateList.Head.switch;
                  -- If the current state is the same as the request state, then return.
                  if switchPtr.state = state or else
                     (switchPtr.state = beginThrown and state = thrown) or else
                     (switchPtr.state = beginClosed and state = closed)        
                  then
                     return;
                  end if;
                  
                  -- Determine if it is possible to move the switch                  
                  MoveSwitchPossible(SectionPtr.Section.mySwitchStateList.Head.switch, TrainId, Result);
                  IF Result THEN
                     -- It is possible
                     IF State = Thrown THEN
                        SectionPtr.Section.mySwitchStateList.Head.Switch.State := BeginThrown;
                     ELSE
                        SectionPtr.Section.mySwitchStateList.Head.Switch.State := BeginClosed;
                     END IF;
                     SendLoseReservationMessages(SectionPtr.Section.mySwitchStateList.Head.switch);          -- mo 1/8/12
                     SendToOutQueue(makePutSwitchStateMsg(SectionPtr.Section.mySwitchStateList.Head.Switch.Id, 
                                                          SectionPtr.Section.mySwitchStateList.Head.Switch.State));
                     SendToOutQueue(makeSwReqMsg(SectionPtr.Section.mySwitchStateList.Head.Switch.Id, State));
                  END IF;
               END IF;
               return;                   -- finised processing the train so done
            END IF;
            TrainPtr := TrainPtr.Next;   -- still looking for the train
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveNextSwitch: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END MoveNextSwitch;

      PROCEDURE MoveSwitch (SwitchId : Positive; State    : SwitchStateType) IS
         SwitchPtr            : SwitchNodePtr := pkgSwitchList.Head;
         Result               : Boolean;
         trainId              : trainIdType;
         thereIsReservation   : boolean;
         found                : boolean;
      BEGIN
         while switchPtr /= null loop
            found := true;
            exit when SwitchPtr.Switch.Id = SwitchId;
            found := false;
            SwitchPtr := SwitchPtr.Next;
         end loop;
         if not found or else
           switchPtr.switch.state = state or else
           (switchPtr.switch.state = beginThrown and state = thrown) or else
           (switchPtr.switch.state = beginClosed and state = closed)        
         then
            return;
         end if;
         
         MoveSwitchPossible(SwitchPtr.switch, Result);
         IF Result THEN
            FindIdOfTrainLoosingReservation(switchPtr, trainId, thereIsReservation);
            if thereIsReservation then
               SendToTrainQueue(makeLoseReservationMsg(TrainId), TrainId);
               -- The train delays when it does a "lose reservation"
               -- This causes the next two messages to be delayed until the train stops.
               sendToTrainQueue(makeSwReqMsg(SwitchPtr.Switch.Id, State), trainId);
            else
               IF State = Thrown THEN
                  SwitchPtr.Switch.State := BeginThrown;
               ELSE
                  SwitchPtr.Switch.State := BeginClosed;
               END IF;
               SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, SwitchPtr.Switch.State));
               SendToOutQueue(makeSwReqMsg(SwitchPtr.Switch.Id, State));
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
         OutSectList : SectionNodeList;     
         SectionPtr  : SectionNodePtr;
      BEGIN
         IF Sensors'Length /= Count OR Count < 2 THEN
            result := false;
            RETURN;
         END IF;
         
         FindSectionsCorrespondingToListOfSensors(OutSectList, Sensors);
         IF not AllFree(OutSectList) THEN
            result := false;
         else
            PlaceTrainInSections(OutSectList, TrainId);  
            
            if isNewTrain(trainId) then                          -- mo 1/18/12
               AddNewTrain(TrainId, Sensors);
            else
               UpdateTrainSensors(trainId, sensors);
            end if;            
            
            SectionPtr := OutSectList.Head;
            WHILE SectionPtr /= NULL LOOP
               BlockSections(SectionPtr.Section.BlockingList);
               SectionPtr := SectionPtr.Next;
            END LOOP;
            Result := True;
         END IF;
        makeEmptySectionNodeList(OutSectList); -- memory leak fixed here
        return;   
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in PositionTrain: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END PositionTrain;

      PROCEDURE ReleaseReservation (
            TrainId : TrainIdType) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved AND
                  SectionPtr.Section.TrainId = TrainId THEN
               SectionPtr.Section.State := Free;
               SectionPtr.Section.trainId := 0;
               ReleaseBlockings(SectionPtr.Section.BlockingList);
               SendToOutQueue(makePutSectionStateMsg(SectionPtr.Section.Id, Free));
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
         prev, curr      : TrainNodePtr;
      begin
         prev := null;
         curr := pkgTrainList;
         while curr /= null loop
            if curr.trainId /= trainId then
               prev := curr;
               curr := curr.next;
            else
               if curr = pkgTrainList then
                  pkgTrainList := pkgTrainList.next;    
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
         ptr := pkgSensorList.head;
         while ptr /= null loop
            if ptr.sensor.state = closed then
               ptr.sensor.state := open;
               SendToOutQueue(makePutSensorStateMsg(ptr.sensor.id, open));
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
         SwitchPtr         : SwitchNodePtr  := pkgSwitchList.Head;
         -- ThrownSectionList : SectionNodeList;
         -- ClosedSectionList : SectionNodeList;
         -- Thrownusable     : Boolean        := False;
         -- Closedusable     : Boolean        := False;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            IF SwitchPtr.Switch.Id = SwitchId THEN
               IF State = Closed THEN
                  SwitchPtr.Switch.State := Closed;
               ELSIF State = Thrown THEN
                  SwitchPtr.Switch.State := Thrown;
               END IF;
               SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, SwitchPtr.Switch.State));
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
         pkgCurrentSection := NEW SectionObj;
         pkgCurrentSection.Id := Id;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldNewSection: " & Exception_Information(Error));
            RAISE;
      END bldNewSection;

      PROCEDURE bldEndSection IS
      BEGIN
         IF pkgSectionList.Head = NULL THEN
            pkgSectionList.Head := NEW SectionNode;
            pkgSectionList.Tail := pkgSectionList.Head;
         ELSE
            pkgSectionList.Tail.Next := NEW SectionNode;
            pkgSectionList.Tail := pkgSectionList.Tail.Next;
         END IF;
         pkgSectionList.Tail.Section := NEW SectionObj;
         pkgSectionList.Tail.Section.All := pkgCurrentSection.All;
         Free_Section(pkgCurrentSection);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldEndSection: " & Exception_Information(Error));
            RAISE;
     END bldEndSection;

      -- Add a section to the end of a section list
      PROCEDURE AddSectionToEndOfList (
            OutSectList : IN OUT SectionNodeList;
            NodePtr     :        SectionNodePtr) IS
      BEGIN
         IF OutSectList.Head = NULL THEN
            OutSectList.Head := NEW SectionNode;
            OutSectList.Tail := OutSectList.Head;
         ELSE
            OutSectList.Tail.Next := NEW SectionNode;
            OutSectList.Tail := OutSectList.Tail.Next;
         END IF;
         OutSectList.Tail.Section := NodePtr.Section;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddSectionToEndOfList: " & Exception_Information(Error));
            RAISE;
      END AddSectionToEndOfList;

      -- check if a section is in a blocking list
      FUNCTION IsIn (
            BlockList : BlockingNodeList;
            Id        : Positive)
        RETURN Boolean IS
         BlockingPtr : BlockingNodePtr := BlockList.Head;
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

      PROCEDURE bldFindAllSections (
            SensorId         :        Positive;
            CurrentSectId    :        Positive;
            CurSectBlockList :        BlockingNodeList;
            OutSectList      :    OUT SectionNodeList) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section /= NULL THEN    --x how could it = null??????????
               IF SectionPtr.Section.Id /= CurrentSectId AND
                  NOT IsIn(CurSectBlockList, SectionPtr.Section.Id) AND
                  SectionPtr.Section.SensorList.Head /= NULL 
               THEN
                  IF SectionPtr.Section.SensorList.Head.Sensor.Id = SensorId OR
                     SectionPtr.Section.SensorList.Tail.Sensor.Id = SensorId 
                  THEN
                     AddSectionToEndOfList(OutSectList, SectionPtr);
                  END IF;
               END IF;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldFindAllSections: " & Exception_Information(Error));
            RAISE;
      END bldFindAllSections;

      -- Set the NextSectionList for each section
      PROCEDURE bldSetNextSectionList IS
         SectionPtr     : SectionNodePtr := pkgSectionList.Head;
         OutSectionList : SectionNodeList;
      BEGIN
         WHILE SectionPtr/= NULL LOOP
            bldFindAllSections(SectionPtr.Section.SensorList.Tail.Sensor.Id,
                               SectionPtr.Section.Id,  
                               SectionPtr.Section.BlockingList,
                               OutSectionList);
            SectionPtr.Section.NextSectionList := OutSectionList;
            OutSectionList.Head := NULL;       --x not memory leak
            OutSectionList.Tail := NULL;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldSetNextSectionList: " & Exception_Information(Error));
            RAISE;
      END bldSetNextSectionList;

      -- Set the PrevSectionList for each section
      PROCEDURE bldSetPrevSectionList IS
         SectionPtr     : SectionNodePtr := pkgSectionList.Head;
         OutSectionList : SectionNodeList;
      BEGIN
         WHILE SectionPtr/= NULL LOOP
            bldFindAllSections(SectionPtr.Section.SensorList.Head.Sensor.Id,
                               SectionPtr.Section.Id, 
                               SectionPtr.Section.BlockingList,
                               OutSectionList);
            SectionPtr.Section.PrevSectionList := OutSectionList;
            OutSectionList.Head := NULL;     -- not a memory leak
            OutSectionList.Tail := NULL;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldSetPrevSectionList: " & Exception_Information(Error));
            RAISE;
      END bldSetPrevSectionList;

      PROCEDURE bldEndSectionList IS
      BEGIN
         bldSetNextSectionList;
         bldSetPrevSectionList;
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
         RETURN (Sensors.Head.Sensor.Id = FirstId AND Sensors.Tail.Sensor.Id = SecondId) OR
            (Sensors.Tail.Sensor.Id = FirstId AND Sensors.Head.Sensor.Id = SecondId);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in CheckSensors: " & Exception_Information(Error));
            raise;
      END CheckSensors;

      PROCEDURE bldGetSectionsContainingSwitch (
            SwitchPtr  :        SwitchObjPtr;
            ThrownList :    OUT SectionNodeList;
            ClosedList :    OUT SectionNodeList) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
         SensorPtr  : SensorNodePtr;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            CASE SwitchPtr.TypeOfSwitch IS
               WHEN Normal =>
                  IF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SwitchPtr.ThrownSensor.Id) THEN
                     AddSectionToEndOfList(ThrownList, SectionPtr);
                  ELSE
                     SensorPtr := SwitchPtr.ClosedSensors.Head;
                     WHILE SensorPtr /= NULL LOOP
                        IF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SensorPtr.Sensor.Id) THEN
                           AddSectionToEndOfList(ClosedList, SectionPtr);
                           SensorPtr := NULL;
                        ELSE
                           SensorPtr := SensorPtr.Next;
                        END IF;
                     END LOOP;
                  END IF;
               WHEN Crossover =>
                  IF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SwitchPtr.NarrowSensors.Tail.Sensor.Id) THEN
                     AddSectionToEndOfList(ThrownList, SectionPtr);
                  ELSIF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SwitchPtr.ClosedSensors.Head.Sensor.Id) OR
                        CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Tail.Sensor.Id, SwitchPtr.ClosedSensors.Tail.Sensor.Id) THEN
                     AddSectionToEndOfList(ClosedList, SectionPtr);
                  END IF;
            END CASE;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldGetSectionsContainingSwitch: " & Exception_Information(Error));
            RAISE;
      END bldGetSectionsContainingSwitch;

      -- Finish things up when the pkgSwitchList is all read in
      PROCEDURE bldEndSwitchList IS
         SwitchPtr         : SwitchNodePtr  := pkgSwitchList.Head;
         ClosedSectionList : SectionNodeList;
         ThrownSectionList : SectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         -- Loop through all switches in the global switch list
         WHILE SwitchPtr /= NULL LOOP
            bldGetSectionsContainingSwitch(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
            IF SwitchPtr.Switch.State = Closed THEN
               -- This switch will be set to CLOSED thus making all the sections on the thrown side
               -- of the switch unusable.
               SectionPtr := ThrownSectionList.Head;
               WHILE SectionPtr /= NULL LOOP
                  SectionPtr.Section.Isusable := False;
                  SectionPtr := SectionPtr.Next;
               END LOOP;
            ELSE
               -- This switch will be set to THROWN thus making all the sections on the closed side
               -- of the switch unusable.
               SectionPtr := ClosedSectionList.Head;
               WHILE SectionPtr /= NULL LOOP
                  SectionPtr.Section.Isusable := False;
                  SectionPtr := SectionPtr.Next;
               END LOOP;
            END IF;
            
            --x Build a list of all sections containing this switch
            sectionPtr := thrownSectionList.head;
            while sectionPtr /= null loop
               AddSectionToEndOfList(switchPtr.switch.sectionNList, sectionPtr);
               sectionPtr := sectionPtr.next;
            end loop;
            sectionPtr := closedSectionList.head;
            while sectionPtr /= null loop
               AddSectionToEndOfList(switchPtr.switch.sectionNList, sectionPtr);
               sectionPtr := sectionPtr.next;
            end loop;
                       
            -- Move the physical switch to the proper setting and inform all OThrottles          
            if SwitchPtr.Switch.State = Read then  --x never happens
               SendToOutQueue(makeSwStateMsg(SwitchPtr.Switch.Id));
            else
               SendToOutQueue(makeSwReqMsg(SwitchPtr.Switch.Id, SwitchPtr.Switch.State));
               if not simulator then 
                  null;
                  -- delay 3.0;             -- don't want to overwhelm the locobuffer server
               end if;
               if  SwitchPtr.Switch.State = Closed then
                  SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, BeginClosed));
               else
                  SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, BeginThrown));
               end if;
            end if;
            
            -- Memory leak fixed here
            makeEmptySectionNodeList(closedSectionList);
            makeEmptySectionNodeList(thrownSectionList);
            
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldEndSwitchList: " & Exception_Information(Error));
            RAISE;
      END bldEndSwitchList;

      PROCEDURE bldFindSwitch (
            Switchs   :        SwitchNodeList;
            SwitchId  :        Positive;
            SwitchPtr :    OUT SwitchNodePtr) IS
      BEGIN
         SwitchPtr := Switchs.Head;
         WHILE SwitchPtr /= NULL LOOP
            IF SwitchPtr.Switch.Id = SwitchId THEN
               RETURN;
            END IF;
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldFindSwitch: " & Exception_Information(Error));
            put_line("    Looking for switch # " & Positive'Image(SwitchId));
            RAISE;
      END bldFindSwitch;

      PROCEDURE bldAddSwitchToSection (
            Id    : Positive;
            state : switchStateType) IS
         globalSwitchNodePtr  : SwitchNodePtr;
         -- localSwitchNodePtr   : SwitchNodePtr;
         switchOPtr           : switchObjPtr;
         
      BEGIN
         -- Create a new SwitchNode and link it into the section's SwitchNodeList
         --x OLD not needed
         -- IF pkgCurrentSection.SwitchList.Head = NULL THEN
            -- pkgCurrentSection.SwitchList.Head := NEW SwitchNode;
            -- pkgCurrentSection.SwitchList.Tail := pkgCurrentSection.SwitchList.Head;
         -- ELSE
            -- pkgCurrentSection.SwitchList.Tail.Next := NEW SwitchNode;
            -- pkgCurrentSection.SwitchList.Tail := pkgCurrentSection.SwitchList.Tail.Next;
         -- END IF;

         -- Look for the switch node in pkgSwitchList (the global list of switch nodes) 
         bldFindSwitch(pkgSwitchList, Id, globalSwitchNodePtr);
         IF globalSwitchNodePtr /= NULL THEN
            -- The node is found: link the section's new SwitchNode to the global switch object
            switchOPtr := globalSwitchNodePtr.Switch;
            --x OLD not needed
            -- pkgCurrentSection.SwitchList.Tail.Switch := switchOPtr;
            null;
         ELSE
            -- The node isn't found: create it and link it into the global pkgSwitchList.
            IF pkgSwitchList.Head = NULL THEN
               pkgSwitchList.Head := NEW SwitchNode;
               pkgSwitchList.Tail := pkgSwitchList.Head;
            ELSE
               pkgSwitchList.Tail.Next := NEW SwitchNode;
               pkgSwitchList.Tail := pkgSwitchList.Tail.Next;
            END IF;
            globalSwitchNodePtr := pkgSwitchList.Tail;
            -- Create a new SwitchObj and link it to both the global pkgSwitchList and the section's
            -- SwitchList.
            switchOPtr := new switchObj;
            switchOPtr.Id := Id;
            globalSwitchNodePtr.Switch := switchOPtr;            -- the global pkgSwitchList
            --x OLD not needed
            -- localSwitchNodePtr := pkgCurrentSection.SwitchList.Tail;
            -- localSwitchNodePtr.Switch := switchOPtr;             -- the section's SwitchList
         END IF;
         
         -- Create a new SwitchStateNode and link it into the section's SwitchStateList
         IF pkgCurrentSection.mySwitchStateList.Head = NULL THEN
            pkgCurrentSection.mySwitchStateList.Head := NEW switchStateNode;
            pkgCurrentSection.mySwitchStateList.Tail := pkgCurrentSection.mySwitchStateList.Head;
         ELSE
            pkgCurrentSection.mySwitchStateList.Tail.Next := NEW switchStateNode;
            pkgCurrentSection.mySwitchStateList.Tail := pkgCurrentSection.mySwitchStateList.Tail.Next;
         END IF;
         pkgCurrentSection.mySwitchStateList.tail.switch := switchOPtr;
         pkgCurrentSection.mySwitchStateList.tail.myState := state;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in bldAddSwitchToSection: " & Exception_Information(Error));
            put_line("    adding switch #" & Positive'Image(Id));
            RAISE;
      END bldAddSwitchToSection;

      PROCEDURE bldUpdateSwitch (
            Id           : Positive;
            TypeOfSwitch : ControllerGlobals.SwitchType;
            state        : switchStateType) IS
         SwitchPtr : SwitchNodePtr;
      BEGIN
         bldFindSwitch(pkgSwitchList, Id, SwitchPtr);
         IF SwitchPtr /= NULL THEN
            pkgCurrentSwitch := SwitchPtr.Switch;
            pkgCurrentSwitch.TypeOfSwitch := TypeOfSwitch;
            pkgCurrentSwitch.state := state;
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
         IF pkgCurrentSwitch /= NULL THEN
            FindSensor(pkgSensorList, NarrowId, SensorPtr);
            IF SensorPtr /= NULL THEN
               IF pkgCurrentSwitch.NarrowSensors.Head = NULL THEN
                  pkgCurrentSwitch.NarrowSensors.Head := NEW SensorNode;
                  pkgCurrentSwitch.NarrowSensors.Tail :=
                     pkgCurrentSwitch.NarrowSensors.Head;
               ELSE
                  pkgCurrentSwitch.NarrowSensors.Tail.Next := NEW SensorNode;
                  pkgCurrentSwitch.NarrowSensors.Tail :=
                     pkgCurrentSwitch.NarrowSensors.Tail.Next;
               END IF;
               pkgCurrentSwitch.NarrowSensors.Tail.Sensor := SensorPtr.Sensor;
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
         IF pkgCurrentSwitch /= NULL THEN
            FindSensor(pkgSensorList, ClosedId, SensorPtr);
            IF SensorPtr /= NULL THEN
               IF pkgCurrentSwitch.ClosedSensors.Head = NULL THEN
                  pkgCurrentSwitch.ClosedSensors.Head := NEW SensorNode;
                  pkgCurrentSwitch.ClosedSensors.Tail :=
                     pkgCurrentSwitch.ClosedSensors.Head;
               ELSE
                  pkgCurrentSwitch.ClosedSensors.Tail.Next := NEW SensorNode;
                  pkgCurrentSwitch.ClosedSensors.Tail :=
                     pkgCurrentSwitch.ClosedSensors.Tail.Next;
               END IF;
               pkgCurrentSwitch.ClosedSensors.Tail.Sensor := SensorPtr.Sensor;
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
            ThrownId : Positive) IS
         SensorPtr : SensorNodePtr;
      BEGIN
         IF pkgCurrentSwitch /= NULL THEN
            FindSensor(pkgSensorList, ThrownId, SensorPtr);
            IF SensorPtr /= NULL THEN
               CASE pkgCurrentSwitch.TypeOfSwitch IS
                  WHEN Normal =>
                     pkgCurrentSwitch.ThrownSensor := SensorPtr.Sensor;
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
         IF pkgCurrentSection.SensorList.Head = NULL THEN
            pkgCurrentSection.SensorList.Head := NEW SensorNode;
            pkgCurrentSection.SensorList.Tail := pkgCurrentSection.SensorList.Head;
         ELSE
            pkgCurrentSection.SensorList.Tail.Next := NEW SensorNode;
            pkgCurrentSection.SensorList.Tail := pkgCurrentSection.SensorList.Tail.Next;
         END IF;

         FindSensor(pkgSensorList, Id, SensorPtr);
         IF SensorPtr = NULL THEN
            IF pkgSensorList.Head = NULL THEN
               pkgSensorList.Head := NEW SensorNode;
               pkgSensorList.Tail := pkgSensorList.Head;
            ELSE
               pkgSensorList.Tail.Next := NEW SensorNode;
               pkgSensorList.Tail := pkgSensorList.Tail.Next;
            END IF;
            SensorPtr := pkgCurrentSection.SensorList.Tail;
            SensorPtr.Sensor := NEW SensorObj;
            SensorPtr.Sensor.Id := Id;
            pkgSensorList.Tail.Sensor := SensorPtr.Sensor;
         ELSE
            pkgCurrentSection.SensorList.Tail.Sensor := SensorPtr.Sensor;
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
         IF pkgCurrentSection.BlockingList.Head = NULL THEN
            pkgCurrentSection.BlockingList.Head := NEW BlockingNode;
            pkgCurrentSection.BlockingList.Tail :=
               pkgCurrentSection.BlockingList.Head;
         ELSE
            pkgCurrentSection.BlockingList.Tail.Next := NEW BlockingNode;
            pkgCurrentSection.BlockingList.Tail :=
               pkgCurrentSection.BlockingList.Tail.Next;
         END IF;
         pkgCurrentSection.BlockingList.Tail.Id := Id;
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
            Output      : File_Type) IS
         SensorPtr : SensorNodePtr := sensors.head;
      BEGIN
         WHILE SensorPtr /= NULL LOOP
            Print("Sensor ID/state: " & Positive'Image(SensorPtr.Sensor.Id) & " " & 
                                        SensorStateType'Image(SensorPtr.Sensor.State),
                                        Indent, Output);
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
         SwitchPtr : SwitchNodePtr := Switchs.head;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            Print("----------------------", Indent, Output);
            Print("Switch ID: " & Positive'Image(SwitchPtr.Switch.Id),
               Indent, Output);
            IF NOT PrintOnlyId THEN
               Print("Switch State: " & SwitchStateType'Image(SwitchPtr.Switch.State), Indent, Output);
               Print("Switch Type: " & ControllerGlobals.SwitchType'Image(SwitchPtr.Switch.TypeOfSwitch), Indent, Output);
               Print("Switch Narrow Sensors:", Indent, Output);
               Print_Sensors(SwitchPtr.Switch.NarrowSensors, Indent + 2, Output);
               Print("Switch Closed Sensors:", Indent, Output);
               Print_Sensors(SwitchPtr.Switch.ClosedSensors, Indent + 2, Output);
               IF SwitchPtr.Switch.ThrownSensor /= NULL THEN    --x how could this be null?????????
                  Print("Switch Thrown Sensor: " & Positive'Image(SwitchPtr.Switch.ThrownSensor.Id), Indent, Output);
               END IF;
               print("All sections containing this switch", indent, output);
               print_sections(switchPtr.switch.sectionNList, indent + 2, output, true);
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
            BlockingList : BlockingNodeList;
            Indent       : Natural;
            Output       : File_Type) IS
         BlockingPtr : BlockingNodePtr := BlockingList.Head;
      BEGIN
         WHILE BlockingPtr /= NULL LOOP
            Print("Blocking ID: " & Positive'Image(BlockingPtr.Id), Indent, Output);
            BlockingPtr := BlockingPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in Print_Blockings: " & Exception_Information(Error));
            RAISE;
      END Print_Blockings;

      PROCEDURE Print_mySwitchStateList (
            mySwitchStateList : switchStateList;
            Indent       : Natural;
            Output       : File_Type) IS
         ptr : SwitchStateNodePtr := mySwitchStateList.Head;
      BEGIN
         WHILE ptr /= NULL LOOP
            Print("Switch ID/state: " & Positive'Image(ptr.switch.Id) & " " & 
                                        SwitchStateType'Image(ptr.myState), Indent, Output);
            ptr := ptr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in Print_mySwitchStateList: " & Exception_Information(Error));
            RAISE;
      END Print_mySwitchStateList;

      PROCEDURE Print_Sections (
            Sections    : SectionNodeList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean        := False) IS
         SectionPtr : SectionNodePtr := Sections.head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            if printOnlyId then
               Print("Section ID: " & Positive'Image(SectionPtr.Section.Id), Indent, Output);
            else
               Print("---------------", Indent, Output);
               Print("Section ID: " & Positive'Image(SectionPtr.Section.Id), Indent, Output);
               Print("Section State: " & SectionStateType'Image(
                     SectionPtr.Section.State), Indent, Output);
               Print("Section Is usable: " & Boolean'Image(SectionPtr.Section.Isusable), Indent, Output);
               IF SectionPtr.Section.State = Occupied OR SectionPtr.Section.State = Reserved THEN
                  Print("Section TrainAddr: " & TrainIdType'Image(SectionPtr.Section.TrainId), Indent, Output);
               END IF;
               IF SectionPtr.Section.State = Blocked THEN
                  Print("Section Block Count:" & Positive'Image(SectionPtr.Section.BlockCount), Indent, Output);
               END IF;
               Print("Section Sensors:", Indent, Output);
               Print_Sensors(SectionPtr.Section.SensorList, Indent + 2, Output);
               --x OLD not needed
               -- Print("Section Switchs:", Indent, Output);
               -- Print_Switchs(SectionPtr.Section.SwitchList, Indent + 2, Output, True);
               print("MySwitchStateList:", indent, output);
               print_mySwitchStateList(SectionPtr.section.mySwitchStateList, indent + 2, output);
               Print("Next Sections:", Indent, Output);
               Print_Sections(SectionPtr.Section.NextSectionList, Indent + 2, Output, True);
               Print("Previous Sections:", Indent, Output);
               Print_Sections(SectionPtr.Section.PrevSectionList, Indent + 2, Output, True);
               Print("Section Blockings:", Indent, Output);
               Print_Blockings(SectionPtr.Section.BlockingList, Indent + 2, Output);
               Print("---------------", Indent, Output);
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in Print_Sections: " & Exception_Information(Error));
            RAISE;
      END Print_Sections;
      
      function getSectionList return sectionNodeList is
      begin 
         return pkgSectionList;
      end;
      
      function getPkgSensorList return sensorNodeList is
      begin 
         return pkgSensorList;
      end; 
      
      function getPkgSwitchList return switchNodeList is
      begin 
         return pkgSwitchList;
      end;

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
            IF SensorPtr.Sensor.Id = SensorId THEN
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
            BlockingList : BlockingNodeList) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
      BEGIN
         IF BlockingList.Head /= NULL THEN
            WHILE SectionPtr /= NULL LOOP
               IF IsIn(BlockingList, SectionPtr.Section.Id) THEN
                  SectionPtr.Section.BlockCount := SectionPtr.Section.BlockCount - 1;
                  IF SectionPtr.Section.BlockCount = 0 THEN
                     SectionPtr.Section.State := Free;
                     SectionPtr.Section.trainId := 0;
                     SendToOutQueue(makePutSectionStateMsg(SectionPtr.Section.Id, Free));
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
         TrainPtr  : TrainNodePtr   := pkgTrainList;
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
            addEnd(sList, sensorPtr.sensor.id);
            sensorPtr := sensorPtr.next;
         end loop;
         return sList;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getTrainsSensorNumbers: " & Exception_Information(Error));
            RAISE;
      end getTrainsSensorNumbers;
      
      -- Add a new sensor to the front of a train's sensor list
      PROCEDURE AddNewSensorToFront (TrainId : TrainIdType; Sensor  : SensorObjPtr) IS
         TrainPtr  : TrainNodePtr   := pkgTrainList;
         SensorPtr : SensorNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               myPutLine("      <<<<<<<<<<< " & integer'image(sensor.id)  & " sensor added to front");
               TrainPtr.SensorCount := TrainPtr.SensorCount + 1;
               SensorPtr := TrainPtr.SensorList.Head;
               TrainPtr.SensorList.Head := NEW SensorNode;
               TrainPtr.SensorList.Head.Sensor := Sensor;
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
         
      FUNCTION GetSensorPtrs (TrainId : TrainIdType) RETURN AccessToArrayOfSensorObjPtrType IS
         TrainPtr  : TrainNodePtr       := pkgTrainList;
         SensorPtr : SensorNodePtr;
         Sensors   : AccessToArrayOfSensorObjPtrType;
         I         : Positive          := 1;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               Sensors := NEW ArrayOfSensorObjPtrType(1..TrainPtr.SensorCount);
               SensorPtr := TrainPtr.SensorList.Head;
               WHILE SensorPtr /= NULL LOOP
                  Sensors(I) := SensorPtr.Sensor;
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
         TrainPtr  : TrainNodePtr       := pkgTrainList;
         SensorPtr : SensorNodePtr;
         Sensors   : SensorArrayAccess;
         I         : Positive          := 1;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               Sensors := NEW SensorArrayType(1..TrainPtr.SensorCount);
               SensorPtr := TrainPtr.SensorList.Head;
               WHILE SensorPtr /= NULL LOOP
                  Sensors(I) := SensorPtr.Sensor.Id;
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
         TrainPtr  : TrainNodePtr       := pkgTrainList;
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
      PROCEDURE getUnblockedUsableSectionsContainingSensor (
            SensorID      :        Positive;
            FirstSection  :    OUT SectionObjPtr;
            SecondSection :    OUT SectionObjPtr) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
      BEGIN
         FirstSection := NULL;
         SecondSection := NULL;
         WHILE SectionPtr /= NULL LOOP
            IF      (SectionPtr.Section.state /= blocked)
            AND THEN(SectionPtr.Section.SensorList.Head.Sensor.Id = SensorId OR
                     SectionPtr.Section.SensorList.Tail.Sensor.Id = SensorId)         
            AND THEN (IsSectionusable(sectionPtr.section))
            THEN
               IF FirstSection = NULL THEN
                  FirstSection := SectionPtr.Section;
               ELSE
                  SecondSection := SectionPtr.Section;
                  RETURN;
               END IF;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getUnblockedUsableSectionsContainingSensor: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END getUnblockedUsableSectionsContainingSensor;

      -- Get the two sections surrounding the sensor that
      --   are either occupied or reserved
      PROCEDURE GetOccResSections (
            SensorID      :        Positive;
            FirstSection  :    OUT SectionObjPtr;
            SecondSection :    OUT SectionObjPtr;
            searchOutcome :    out natural) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
      BEGIN
         FirstSection := NULL;
         SecondSection := NULL;
         
         -- Try to find two sections that are occupied/reserved and which contain the sensor
         WHILE SectionPtr /= NULL LOOP
            IF   (SectionPtr.Section.SensorList.Head.Sensor.Id = SensorId OR
                  SectionPtr.Section.SensorList.Tail.Sensor.Id = SensorId) 
            AND  (SectionPtr.Section.State = Occupied OR SectionPtr.Section.State = Reserved) 
            THEN
               IF FirstSection = NULL THEN
                  FirstSection := SectionPtr.Section;
               else
                  secondSection := sectionPtr.section;
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
         TrainPtr  : TrainNodePtr   := pkgTrainList;
         SensorPtr : SensorNodePtr;
         sPtr      : sensorNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               TrainPtr.SensorCount := TrainPtr.SensorCount - 1;
               SensorPtr := TrainPtr.SensorList.Head;
               WHILE SensorPtr /= NULL LOOP
                  IF SensorPtr.Next = TrainPtr.SensorList.Tail THEN
                     myPutLine("      <<<<<<<<<<< " & integer'image(sensorPtr.next.sensor.id) & " sensor removed from tail" );
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
      function  getSensorAtFrontOfReservedSectionPtr(section : sectionObj) return sensorObjPtr is
         trainId   : trainIdType;
         ptr       : sensorObjPtr;      -- train's front sensor ptr
      BEGIN
         trainId := section.trainId;
         ptr := getFrontSensorPtr(trainId);
         if ptr = section.sensorList.head.sensor then
            return section.sensorList.tail.sensor;
         else   
            return section.sensorList.head.sensor;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getSensorAtFrontOfReservedSectionPtr: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end getSensorAtFrontOfReservedSectionPtr;

      -- Get the sensor at the front of a train's reserved section
      function  getSensorAtFrontOfReservedSection(section : sectionObj) return positive is
         trainId   : trainIdType;
         s1        : positive;      -- train's front sensor
      BEGIN
         trainId := section.trainId;
         s1 := getFrontSensor(trainId);
         if s1 = section.sensorList.head.sensor.id then
            return section.sensorList.tail.sensor.id;
         else   
            return section.sensorList.head.sensor.id;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in getSensorAtFrontOfReservedSection: " & Exception_Information(Error));
            put_line("    train id #" & Positive'Image(trainId));
            RAISE;
      end getSensorAtFrontOfReservedSection;

      -- Get a train's front sensor ptr
      function GetFrontSensorPtr(TrainId : TrainIdType) return sensorObjPtr is
         TrainPtr : TrainNodePtr := pkgTrainList;
         ptr      : sensorObjPtr := null;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               ptr := TrainPtr.SensorList.head.sensor;
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
      function GetBackSensorPtr(TrainId : TrainIdType) return sensorObjPtr is
         TrainPtr : TrainNodePtr := pkgTrainList;
         ptr      : sensorObjPtr := null;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               ptr := TrainPtr.SensorList.tail.sensor;
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
         TrainPtr : TrainNodePtr := pkgTrainList;
         frontId   : positive;
      BEGIN
         frontId := Positive'Last;
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               frontId := TrainPtr.SensorList.head.Sensor.Id;
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
         TrainPtr : TrainNodePtr := pkgTrainList;
         backId   : positive;
      BEGIN
         BackId := Positive'Last;
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               BackId := TrainPtr.SensorList.Tail.Sensor.Id;
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
         TrainPtr : TrainNodePtr := pkgTrainList;
      BEGIN
         BackId := Positive'Last;
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               BackId := TrainPtr.SensorList.Tail.Sensor.Id;
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
         -- SensorPtr.Sensor.StartTime := Clock;
         if sensorPtr.sensor.state = open then
            sensorPtr.sensor.state := closed;
         else
            sensorPtr.sensor.state := open;
         end if;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in flipSensor: " & Exception_Information(Error));
            put_line("    sensor id #" & Positive'Image(SensorPtr.Sensor.Id));
            RAISE;
      end flipSensor;

      procedure errorStopTrainsAtBadSensor(idSensorCase         : positive;
                                           sx                   : positive;
                                           leftSectionPtr       : sectionObjPtr;
                                           rightSectionPtr      : sectionObjPtr) is
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

      function trainInSection(trainId : trainIdtype; sp : sectionObjPtr) return boolean is
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

      function sectionReserved(sp : sectionObjPtr) return boolean is
      begin
         if sp /= null and then sp.state = reserved then
            return true;
         else
            return false;
         end if;
      end sectionReserved;
      
      function sectionOccupied(sp : sectionObjPtr) return boolean is
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
                               leftSectionPtr   : out sectionObjPtr;
                               rightSectionPtr  : out sectionObjPtr) is
         sensorPtr                  : sensorObjPtr;
         trainPtr                   : TrainNodePtr;
         sId                        : positive;
         section1Ptr, section2Ptr   : sectionObjPtr;
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
         trainPtr := pkgTrainList;
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
         trainPtr := pkgTrainList;
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
         trainPtr := pkgTrainList;
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
         TrainPtr : TrainNodePtr := pkgTrainList;
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
         TrainPtr : TrainNodePtr := pkgTrainList;
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
      FUNCTION IsSectionusable (SectionPtr : SectionObjPtr) RETURN Boolean IS
         -- SwitchPtr        : SwitchNodePtr := SectionPtr.SwitchList.Head;  --x OLD
         switchStateNPtr  : SwitchStateNodePtr := sectionPtr.mySwitchStateList.head;
         -- Result           : Boolean;      -- := False;                         -- mo 1/6/12
         -- SensorPtr        : SensorNodePtr;
         -- Id               : Positive;
      BEGIN
         --x NEW since I added mySwitchStateList to SectionObj
         while switchStateNPtr /= null loop
            if switchStateNPtr.myState /= switchStateNPtr.switch.state then
               return false;
            end if;
            switchStateNPtr := switchStateNPtr.next;
         end loop;
         return true;
                 
         --x OLD which will be thrown away once the NEW one works
         -- Loop through all switches in this section to see 
         -- if they are all set correctly for usability of this section.
         -- WHILE SwitchPtr /= NULL LOOP
            -- -- Assume the section is not usable because this switch is set incorrectly wrong
            -- result := false;                                             -- mo 1/6/12
            -- CASE SwitchPtr.Switch.TypeOfSwitch IS
               -- WHEN Normal =>
                  -- -- The switch is normal
                  -- -- Let Id = the section sensor that is at the closed/thrown end of the switch
                  -- IF SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id THEN
                     -- Id := SectionPtr.SensorList.Tail.Sensor.Id;
                  -- ELSE
                     -- Id := SectionPtr.SensorList.Head.Sensor.Id;
                  -- END IF;
                  -- CASE SwitchPtr.Switch.State IS
                     -- WHEN Closed =>
                        -- -- The switch is closed
                        -- SensorPtr := SwitchPtr.Switch.ClosedSensors.Head;
                        -- WHILE SensorPtr /= NULL LOOP
                           -- IF SensorPtr.Sensor.Id = Id THEN
                              -- -- Sensor Id matches one of the sensors in the switch's closed list
                              -- -- Therefore from the perspective of this switch the section is usable.
                              -- Result := True;
                              -- exit;
                           -- END IF;
                           -- SensorPtr := SensorPtr.Next;
                        -- END LOOP;
                        -- IF NOT Result THEN
                           -- -- Sensor Id did not match one of the sensors in the switch's closed list
                           -- -- so the section is NOT usable
                           -- RETURN False;
                        -- END IF;
                     -- WHEN Thrown =>
                        -- -- The switch is thrown
                        -- IF Id /= SwitchPtr.Switch.ThrownSensor.Id THEN
                           -- -- Sensor Id did not match the sensor at the switch's thrown end
                           -- -- so the section is NOT usable
                           -- RETURN False;
                        -- END IF;
                     -- WHEN OTHERS =>
                        -- -- The switch is moving, so the section is NOT usable
                        -- RETURN False;
                  -- END CASE;
               -- WHEN Crossover =>
                  -- -- The switch is part of a crossover pair
                  -- CASE SwitchPtr.Switch.State IS
                     -- WHEN Closed =>
                        -- IF NOT (SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              -- SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Head.Sensor.Id) AND
                              -- NOT (SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              -- SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Head.Sensor.Id) AND
                              -- NOT (SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id AND
                              -- SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Tail.Sensor.Id) AND
                              -- NOT (SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id AND
                              -- SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Tail.Sensor.Id) THEN
                           -- RETURN False;
                        -- END IF;
                     -- WHEN Thrown =>
                        -- IF NOT (SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              -- SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id) AND
                              -- NOT (SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              -- SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id) THEN
                           -- RETURN False;
                        -- END IF;
                     -- WHEN OTHERS =>
                        -- RETURN False;
                  -- END CASE;
            -- END CASE;
            -- SwitchPtr := SwitchPtr.Next;
         -- END LOOP;
         -- RETURN True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IsSectionusable: " & Exception_Information(Error));
            raise;
      END IsSectionusable;

      FUNCTION AllFree (SectList : SectionNodeList) RETURN Boolean IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         IF SectionPtr = NULL THEN
            RETURN False;
         END IF;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State /= Free OR NOT IsSectionusable(SectionPtr.Section) THEN
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
         SectionPtr := pkgSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.SensorList.Head.Sensor.Id = FirstId
                  AND SectionPtr.Section.SensorList.Tail.Sensor.Id = SecondId THEN
               RETURN;
            ELSIF SectionPtr.Section.SensorList.Head.Sensor.Id = SecondId
                  AND SectionPtr.Section.SensorList.Tail.Sensor.Id = FirstId THEN
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
      PROCEDURE FindSectionsCorrespondingToListOfSensors (OutSectList :    OUT SectionNodeList;
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
               AddSectionToEndOfList(OutSectList, SectionPtr);
            END IF;
            FirstId := SecondId;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindSectionsCorrespondingToListOfSensors: " & Exception_Information(Error));
            raise;
      END FindSectionsCorrespondingToListOfSensors;

      -- Place the train
      PROCEDURE PlaceTrainInSections (
            SectList : SectionNodeList;
            TrainId  : TrainIdType) IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            SectionPtr.Section.State := Occupied;
            SectionPtr.Section.TrainId := TrainId;
            SendToOutQueue(makePutSectionStateMsg(SectionPtr.Section.Id, Occupied));
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in PlaceTrainInSections: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END PlaceTrainInSections;

      PROCEDURE BlockSections (BlockingList : BlockingNodeList) IS
         SectionPtr : SectionNodePtr := pkgSectionList.Head;
      BEGIN
         IF BlockingList.Head /= NULL THEN
            WHILE SectionPtr /= NULL LOOP
               IF IsIn(BlockingList, SectionPtr.Section.Id) THEN
                  SectionPtr.Section.State := Blocked;
                  SectionPtr.Section.BlockCount := SectionPtr.Section.BlockCount + 1;
                  SendToOutQueue(makePutSectionStateMsg(SectionPtr.Section.Id, Blocked));
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
            Sensor   :    OUT SensorObjPtr) IS
         SensorPtr : SensorNodePtr := pkgSensorList.Head;
      BEGIN
         sensor := null;
         WHILE SensorPtr /= NULL LOOP
            IF SensorPtr.Sensor.Id = SensorId THEN
               Sensor := SensorPtr.Sensor;
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
         TrainPtr  : TrainNodePtr := pkgTrainList;
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
            Sections : SectionNodeList;
            Id       : Positive)
        RETURN Boolean IS
         SectionPtr : SectionNodePtr := Sections.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.Id = Id THEN
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
            SectList   :        SectionNodeList;
            OutSectPtr :    OUT SectionObjPtr) IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Free 
            AND IsSectionusable(SectionPtr.Section) THEN
               OutSectPtr := SectionPtr.Section;
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
         TrainPtr : TrainNodePtr := pkgTrainList;
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


      -- PROCEDURE SetNotusable (
            -- SwitchPtr : SwitchNodePtr) IS
         -- ThrownSectionList : SectionNodeList;
         -- ClosedSectionList : SectionNodeList;
         -- SectionPtr        : SectionNodePtr;
      -- BEGIN
         -- bldGetSectionsContainingSwitch(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- SectionPtr.Section.Isusable := False;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- SectionPtr.Section.Isusable := False;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
      -- EXCEPTION
         -- WHEN Error : OTHERS =>
            -- put_line("**************** EXCEPTION Layout pkg in SetNotusable: " & Exception_Information(Error));
            -- raise;
      -- END SetNotusable;

      PROCEDURE GetSection (
            SectionPtr    :    OUT SectionNodePtr;
            FrontSensorId :        Positive;
            BackSensorId  :        Positive) IS
      BEGIN
         SectionPtr := pkgSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF (SectionPtr.Section.SensorList.Head.Sensor.Id = FrontSensorId AND
                  SectionPtr.Section.SensorList.Tail.Sensor.Id = BackSensorId) OR
                  (SectionPtr.Section.SensorList.Head.Sensor.Id = BackSensorId AND
                  SectionPtr.Section.SensorList.Tail.Sensor.Id = FrontSensorId) THEN
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
            SwitchPtr :        SwitchObjPtr;
            TrainId   :        TrainIdType;
            Result    :    OUT Boolean) IS
         -- ThrownSectionList : SectionNodeList;
         -- ClosedSectionList : SectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         --x NEW version
         sectionPtr := switchPtr.sectionNList.head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied OR
                  (SectionPtr.Section.State = Reserved AND
                  SectionPtr.Section.TrainId /= TrainId) THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         result := true;
         return;

         --x OLD can delete
         -- bldGetSectionsContainingSwitch(SwitchPtr, ThrownSectionList, ClosedSectionList);
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Occupied OR
                  -- (SectionPtr.Section.State = Reserved AND
                  -- SectionPtr.Section.TrainId /= TrainId) THEN
               -- Result := False;
               -- RETURN;
            -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Occupied OR
                  -- (SectionPtr.Section.State = Reserved AND
                  -- SectionPtr.Section.TrainId /= TrainId) THEN
               -- Result := False;
               -- RETURN;
            -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- Result := True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveSwitchPossible: " & Exception_Information(Error));
            put_line("    trainId" & integer'image(trainId));
            raise;
      END MoveSwitchPossible;

      -- Is it possible to move this switch? Yes, if no section containing the switch is occupied.
      PROCEDURE MoveSwitchPossible (
            SwitchPtr :        SwitchObjPtr;
            Result    :    OUT Boolean) IS
         -- ThrownSectionList : SectionNodeList;
         -- ClosedSectionList : SectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         --x NEW version
         sectionPtr := switchPtr.sectionNList.head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         result := true;
         return;
         
         --x OLD can delete
         -- bldGetSectionsContainingSwitch(SwitchPtr, ThrownSectionList, ClosedSectionList);
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Occupied THEN
               -- Result := False;
               -- RETURN;
            -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Occupied THEN
               -- Result := False;
               -- RETURN;
            -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- Result := True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveSwitchPossible 2: " & Exception_Information(Error));
            raise;
      END MoveSwitchPossible;

      PROCEDURE FindIdOfTrainLoosingReservation (SwitchPtr : SwitchNodePtr; 
                                                 trainId : out TrainIdType; 
                                                 thereIsReservation : out boolean) IS
         -- ThrownSectionList : SectionNodeList;
         -- ClosedSectionList : SectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         thereIsReservation := false;
         trainId := trainIdType'first;
         
         --x NEW version
         sectionPtr := switchPtr.switch.sectionNList.head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved THEN
               thereIsReservation := true;
               trainId := SectionPtr.Section.TrainId;
               return;
               END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         return;
         
         --x OLD can delete
         -- bldGetSectionsContainingSwitch(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Reserved THEN
               -- thereIsReservation := true;
               -- trainId := SectionPtr.Section.TrainId;
               -- return;
               -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Reserved THEN
               -- thereIsReservation := true;
               -- trainId := SectionPtr.Section.TrainId;
               -- return;
            -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in FindIdOfTrainLoosingReservation: " & Exception_Information(Error));
            raise;
      END FindIdOfTrainLoosingReservation;

      PROCEDURE SendLoseReservationMessages (SwitchPtr : SwitchObjPtr) IS
         -- ThrownSectionList : SectionNodeList;
         -- ClosedSectionList : SectionNodeList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         --x NEW version
         sectionPtr := switchPtr.sectionNList.head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved THEN   --x at most one section should be reserved
               SendToTrainQueue(makeLoseReservationMsg(SectionPtr.Section.TrainId), SectionPtr.Section.TrainId);
               return;
               END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         return;   --x none of the sections containing this switch might have been reserved
          
         -- --x OLD can delete
         -- bldGetSectionsContainingSwitch(SwitchPtr, ThrownSectionList, ClosedSectionList);  
                  -- --x memory leak whenever this procedure is called unless the two lists are
                  -- --x disposed of corretly                                                                               
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Reserved THEN
               -- SendToTrainQueue(makeLoseReservationMsg(SectionPtr.Section.TrainId), SectionPtr.Section.TrainId);
               -- return;
               -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- IF SectionPtr.Section.State = Reserved THEN
               -- SendToTrainQueue(makeLoseReservationMsg(SectionPtr.Section.TrainId), SectionPtr.Section.TrainId);
               -- return;
            -- END IF;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
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
               
      procedure disposeSectionNode(ptr : in out sectionNodePtr) is
      begin
         if ptr /= null then
            disposeBasicSectionNode(ptr);
         end if;
      end disposeSectionNode;
               
      procedure makeEmptySensorNodeList(snl : in out SensorNodeList) is 
         cur, temp  : sensorNodePtr;
      begin
         cur := snl.head;
         while cur /= null loop
            temp := cur;
            cur := cur.next;
            disposeSensorNode(temp);
         end loop;
         snl.head := null;
         snl.tail := null;
      end makeEmptySensorNodeList;
      
      procedure makeEmptySectionNodeList(snl : in out SectionNodeList) is 
         cur, temp  : sectionNodePtr;
      begin
         cur := snl.head;
         while cur /= null loop
            temp := cur;
            cur := cur.next;
            disposeSectionNode(temp);
         end loop;
         snl.head := null;
         snl.tail := null;
      end makeEmptySectionNodeList;
      
      -- procedure disposeTrainNode(ptr : in out TrainNodePtr) is
      -- begin
         -- if ptr /= null then
            -- makeEmptySensorNodeList(ptr.sensorList);
            -- disposeBasicTrainNode(ptr);
         -- end if;
      -- end disposeTrainNode;

      procedure updateTrainSensors(TrainId : TrainIdType; Sensors : SensorArrayType) IS
      -- pre TrainId is in pkgTrainList
         TrainPtr  : TrainNodePtr := pkgTrainList;
         SensorPtr : SensorObjPtr;
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
            TrainPtr.SensorList.Tail.Sensor := SensorPtr;
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
         TrainPtr  : TrainNodePtr;
         SensorPtr : SensorObjPtr;
      BEGIN
         IF pkgTrainList = NULL THEN
            pkgTrainList := NEW TrainNode;
            pkgTrainList.TrainId := TrainId;
            CommandQueueManager.TrainIdQueueList.GetQueue(TrainId, pkgTrainList.Queue);
            TrainPtr := pkgTrainList;
         ELSE
            TrainPtr := pkgTrainList;
            WHILE TrainPtr.Next /= NULL LOOP
               TrainPtr := TrainPtr.Next;
            END LOOP;
            TrainPtr.Next := NEW TrainNode;
            TrainPtr.Next.TrainId := TrainId;
            CommandQueueManager.TrainIdQueueList.GetQueue(TrainId, TrainPtr.Next.Queue);
            TrainPtr := TrainPtr.Next;
         END IF;

         TrainPtr.SensorCount := Sensors'Length;
         TrainPtr.SensorList.Head := NEW SensorNode;
         TrainPtr.SensorList.Tail := TrainPtr.SensorList.Head;
         FOR I IN Sensors'RANGE LOOP
            GetSensor(Sensors(I), SensorPtr);
            TrainPtr.SensorList.Tail.Sensor := SensorPtr;
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
                           when doMakeSectionUsable =>
                              declare
                                 sensor1, sensor2 : positive;
                              begin
                                 splitDoMakeSectionUsableMsg(cmd, sensor1, sensor2);
                                 LayoutPtr.MakeSectionUsable(sensor1, sensor2);
                              end;
                           when getTrainPosition =>
                              declare
                                 trainId : trainIdType;
                              begin
                                 splitGetTrainPositionMsg(cmd, trainId);
                                 layoutPtr.getTrainPosition(trainId);
                              end;
                           when getPath =>
                              declare
                                 preSensor, fromSensor, toSensor : positive;
                                 slotNum                         : slotType;
                                 pathKind                        : pathType;
                                 sensorsToExclude                : naturalListType;
                              begin
                                 splitGetPathMsg(cmd, slotNum, pathKind, preSensor, fromSensor, toSensor, sensorsToExclude);
                                 if pathKind = kDepthFirst then
                                    LayoutPtr.GetPathDF(preSensor, fromSensor, toSensor, sensorsToExclude);
                                 elsif pathKind = kBreadthFirst then
                                    LayoutPtr.GetPathBF(preSensor, fromSensor, toSensor, sensorsToExclude);
                                 end if;
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
   
	function toString(e : sectionNodePtr) return string is
   begin
      return "x";
   end;
   

-------------------- End LayoutTaskType --------------------------
-------------------------- 3 -------------------------------------   
-------------------------- 3 -------------------------------------   
-------------------------- 3 -------------------------------------   
END LayoutPkg;
