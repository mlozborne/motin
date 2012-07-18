WITH MessageTranslationLibrary;
WITH Input_Sources.File, XMLParser, Ada.Exceptions, CommandQueueManager, Interfaces;
USE Input_Sources.File, XMLParser, Ada.Exceptions, CommandQueueManager, Interfaces;
USE MessageTranslationLibrary;
with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
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
      --delay 0.001;        -- mo 12/20/11            test 2
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
            myPutLine("    count" & integer'image(count));
            raise;
      END AreTrainSensorsLegal;

      -- Change a train's direction
      PROCEDURE ChangeDirectionOf (TrainId : TrainIdType) IS
         TrainPtr      : TrainObjPtr   := TrainList;
         ThisSensorPtr : SensorNodePtr;
         PrevSensorPtr : SensorNodePtr := null; 
         NextSensorPtr : SensorNodePtr;
			sList		     : naturalListType;
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
                     mySensors(I) := sensorPtr.sensor.id;
                     I := I + 1;
                     SensorPtr := SensorPtr.Next;
                  END LOOP;
				  
						convertSensorArrayToList(mySensors, sList);
                  SendToOutQueue(makePutTrainPositionMsg(TrainId, sList));
						makeEmpty(sList);
                  disposeSensorArray(mySensors);
				  
               end;
               RETURN;
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in ChangeDirectionOf: " & Exception_Information(Error));
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END ChangeDirectionOf;

      procedure freeAllSectionsOccupiedOrReservedByTrain(trainId : TrainIdType) is
         SectionPtr  : SectionNodePtr;
      begin        
         sectionPtr := sectionList.head;              
         while sectionPtr /= null loop
            if sectionPtr.section.trainId = trainId and (sectionPtr.section.state = occupied or sectionPtr.section.state = reserved) then
               SectionPtr.Section.State := Free;
               ReleaseBlockings(SectionPtr.Section.BlockingList);
               SendToOutQueue(makePutSectionStateMsg(SectionPtr.Section.Id, Free));
            end if;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in freeAllSectionsOccupiedOrReservedByTrain: " & Exception_Information(Error));
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      end freeAllSectionsOccupiedOrReservedByTrain;
		
      PROCEDURE GetSwitchStates IS
         SwitchPtr : SwitchNodePtr := SwitchList.Head;
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
		
      PROCEDURE OldIdentifyTrain (SensorID : Positive) IS
         SensorPtr            : SensorNodePtr;     -- ptr to sensor that fired
			NextSensorPtr        : SensorObjPtr;
         FirstSection         : SectionObjPtr;
         SecondSection        : SectionObjPtr;
			reservedSection      : sectionObjPtr;
			occupiedSection      : sectionObjPtr;
			lastSection          : sectionObjPtr;
         BackId               : Positive;
			sList			         : naturalListType;
			oldSensorState       : sensorStateType;
			newSensorState       : sensorStateType;
			trainId              : trainIdType;
			sensorsPtr           : sensorArrayAccess;
      BEGIN
		   -- Get a pointer to the sensor object.
			-- If not found then ignore this sensor and return
         FindSensor(SensorList, SensorID, SensorPtr);         
         IF SensorPtr = NULL THEN
            myPutLine("      -------------: ERROR(maybe) sensor not recognized " & integer'image(sensorId) ); 
            return;
         end if;
			
         -- Keep track of the old and new state of the sensor
			oldSensorState := sensorPtr.sensor.state;
			flipSensor(sensorPtr);
			newSensorState := sensorPtr.sensor.state;
			
         -- Inform Othrottles that sensor has fired and its new state                 -- mo 1/30/12
         SendToOutQueue(makePutSensorStateMsg(SensorId, newSensorState));
			
			-- Find the two sections that contain the sensor
			getSectionsContainingSensor(SensorPtr.Sensor.Id, FirstSection, SecondSection);
			
			-- If either section is not defined then put all trains in an error state and return
			if firstSection = null or secondSection = null then
            myPutLine("      -------------: ERROR can't find sections for sensor " & integer'image(sensorId) ); 
				SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            return;
         end if;
			
			-- If neither section is occupied then put all trains in an error state and return
			if firstSection = null or secondSection = null then
            myPutLine("      -------------: ERROR no trains nearby " & integer'image(sensorId) ); 
				SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            return;
         end if;
			
			-- If the sections involve different trains then put both trains in an error state and return
			if firstSection.TrainId /= secondSection.TrainId then
            myPutLine("      -------------: ERROR sensor involves two trains" & integer'image(sensorId) ); 
				sendToTrainQueue(makeSensorErrorMsg(SensorId), firstSection.trainId);
				sendToTrainQueue(makeSensorErrorMsg(SensorId), secondSection.trainId);
            return;
         end if;
			
			-- Identify the train and get its array of sensors
			trainId := firstSection.trainId;
			sensorsPtr := GetSensors(TrainId);
			
			-- Identify the fired sensor relative to the train's sensors
			if sensorId = sensorsPtr(1) then
			
				-- First sensor fired
				-- Front of train hit sensor
            myPutLine("      -------------: front of train hit sensor " & integer'image(sensorId)); 
				
				-- Determine which section is reserved, which occupied
				if firstSection.state = reserved and secondSection.state = occupied then
					reservedSection := firstSection;
					occupiedSection := secondSection;
				elsif firstSection.state = occupied and secondSection.state = reserved then
					reservedSection := secondSection;
					occupiedSection := firstSection;
				else
					myPutLine("      -------------: ERROR should have had one section occupied, one reserved " & integer'image(sensorId) ); 
					sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
					disposeSensorArray(sensorsPtr);
					return;
				end if;
				
				-- Determine the next sensor in front of train
				if reservedSection.sensorList.head.sensor.id = sensorId then
					nextSensorPtr := reservedSection.sensorList.tail.sensor;
				else	
					nextSensorPtr := reservedSection.sensorList.head.sensor;
				end if;
				
            -- Change reserved section to occupied.
				-- Send section state message.
				-- Add new sensor to front of train.
				-- Tell train front sensor fired.
				reservedSection.State := Occupied;
            SendToOutQueue(makePutSectionStateMsg(reservedSection.Id, Occupied));				
				AddNewSensorToFront(TrainId, nextSensorPtr);
				SendToTrainQueue(makeFrontSensorFiredMsg(TrainId), TrainId);

				-- Send train position message.
				convertSensorArrayToList(sensorsPtr, sList); 
				SendToOutQueue(makePutTrainPositionMsg(FirstSection.TrainId, sList)); 
				makeEmpty(sList);
				
				disposeSensorArray(sensorsPtr);				
				return;
				
			elsif sensorId = sensorsPtr(sensorsPtr'last - 1) then
			
				-- Second to last sensor fired
				-- Back of train hit sensor
				myPutLine("      -------------: back of train hit sensor " & integer'image(sensorId) ); 
				
				-- Error return if both sections not occupied
				if firstSection.state /= occupied and secondSection.state /= occupied then
					myPutLine("      -------------: ERROR should have both sections occupied " & integer'image(sensorId) ); 
					sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
					disposeSensorArray(sensorsPtr);
					return;
				end if;

				-- Determine which section is last								
				GetBackSensor(FirstSection.TrainId, BackId);
				IF FirstSection.SensorList.Head = SensorPtr OR FirstSection.SensorList.Tail = SensorPtr  THEN
					lastSection := firstSection;
				else
					lastSection := secondSection;
				end if;
				
				-- Free last section and remove it from blocking lists
				-- Send section state message
				-- Remove sensor from back of train
				-- Tell train back sensor fired
				lastSection.state := free;
				releaseBlockings(lastSection.blockingList);
				SendToOutQueue(makePutSectionStateMsg(lastSection.Id, Free));
				RemoveLastSensor(TrainId);
				SendToTrainQueue(makeBackSensorFiredMsg(TrainId), TrainId);

				-- Send train position message
				convertSensorArrayToList(sensorsPtr, sList); 
				SendToOutQueue(makePutTrainPositionMsg(FirstSection.TrainId, sList)); 
				makeEmpty(sList);
		  
		      -- Tell all trains to try to move again
				SendToAllTrainQueues(makeTryToMoveAgainMsg);			
				
				disposeSensorArray(sensorsPtr);
            return;
				
			elsif sensorId = sensorsPtr(2) then
			
				-- Second sensor fired.
				-- Ignore it and return
            myPutLine("      -------------: ignoring train's second sensor fired " & integer'image(sensorId)); 
				disposeSensorArray(sensorsPtr);
				return;

			elsif sensorId = sensorsPtr(sensorsPtr'last) then
			
				-- Last sensor fired.
				-- Ignore it and return
            myPutLine("      -------------: ignoring train's last sensor fired " & integer'image(sensorId)); 
				disposeSensorArray(sensorsPtr);
				return;
				
			else
			
				-- Sensor too far under the train
				-- Put the train in an error state and return
				myPutLine("      -------------: ERROR sensor too far under train " & integer'image(sensorId) ); 
				sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);
				disposeSensorArray(sensorsPtr);
				return;				

			end if;
			
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IdentifyTrain: " & Exception_Information(Error));
            myPutLine("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END OldIdentifyTrain;

      PROCEDURE IdentifyTrain (SensorID : Positive) IS
         SensorPtr        : SensorNodePtr;
         FirstSection     : SectionObjPtr;
         SecondSection    : SectionObjPtr;
         BackId           : Positive;
			sList			     : naturalListType;
			oldSensorState   : sensorStateType;
			newSensorState   : sensorStateType;
      BEGIN
         FindSensor(SensorList, SensorID, SensorPtr);
         
         IF SensorPtr = NULL THEN
            myPutLine("      -------------: ERROR(maybe) sensor not recognized " & integer'image(sensorId) ); 
            return;
         end if;
			
			oldSensorState := sensorPtr.sensor.state;
			flipSensor(sensorPtr);
			newSensorState := sensorPtr.sensor.state;
			
         -- Inform Othrottles that sensor has fired and its new state                 -- mo 1/30/12
         SendToOutQueue(makePutSensorStateMsg(SensorId, newSensorState));
			
         -- if not Simulator and Clock - SensorPtr.Sensor.StartTime < 1.0 then
            -- myPutLine("      -------------: ERROR(maybe) sensor fired too recently " & integer'image(sensorId) ); 
            -- return;
         -- end if;
                     
         -- Get occupied and reserved sections that contain this sensor
         GetOccResSections(SensorPtr.Sensor.Id, FirstSection, SecondSection);
         IF FirstSection /= NULL AND SecondSection /= NULL THEN
            IF FirstSection.State = Occupied AND SecondSection.State = Occupied THEN  
               -- Sensor at back of train
               IF oldSensorState = closed THEN
                  -- Sensor now open
                  -- This should never happen
                  myPutLine("      -------------: ERROR seems like back of train leaving sensor " & integer'image(sensorId) ); 
                  SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
               else 
                  -- Sensor now closed
                  -- First time the sensor fired, back of train approaching sensor
                  myPutLine("      -------------: back of train approaching sensor " & integer'image(sensorId) ); 
                  GetBackSensor(FirstSection.TrainId, BackId);
                  IF (FirstSection.SensorList.Head.Sensor = SensorPtr.Sensor AND FirstSection.SensorList.Tail.Sensor.Id = BackId)
                  OR (FirstSection.SensorList.Tail.Sensor = SensorPtr.Sensor AND FirstSection.SensorList.Head.Sensor.Id = BackId) 
						THEN
                     FirstSection.State := Free;
                     ReleaseBlockings(FirstSection.BlockingList);
                     SendToOutQueue(makePutSectionStateMsg(FirstSection.Id, Free));
                  ELSIF (SecondSection.SensorList.Head.Sensor = SensorPtr.Sensor AND SecondSection.SensorList.Tail.Sensor.Id = BackId)
                  OR (SecondSection.SensorList.Tail.Sensor = SensorPtr.Sensor AND SecondSection.SensorList.Head.Sensor.Id = BackId) 
						THEN
                     SecondSection.State := Free;
                     ReleaseBlockings(SecondSection.BlockingList);
                     SendToOutQueue(makePutSectionStateMsg(SecondSection.Id, Free));
                  ELSE
                     -- Error: sensor id does not match sensor at back of train
                     myPutLine("      -------------: ERROR SensorId " & Positive'image(sensorId) & 
                              " does not match BackId " & Positive'Image(BackId));
                     SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
                     RETURN;
                  END IF;
                  RemoveLastSensor(FirstSection.TrainId);
                  SendToTrainQueue(makeBackSensorFiredMsg(FirstSection.TrainId), FirstSection.TrainId);

                  declare
                     sensorsPtr : sensorArrayAccess;
                  begin
                     sensorsPtr := GetSensors(FirstSection.TrainId);
                     convertSensorArrayToList(sensorsPtr, sList); 
                     SendToOutQueue(makePutTrainPositionMsg(FirstSection.TrainId, sList)); 
                     makeEmpty(sList);
                     disposeSensorArray(sensorsPtr);
                  end;
				  
                  SendToAllTrainQueues(makeTryToMoveAgainMsg);
               END IF;
               -- SendToOutQueue(makePutSensorStateMsg(SensorId, SensorPtr.Sensor.State));             -- mo 1/30/12
            ELSIF FirstSection.State = Reserved OR SecondSection.State = Reserved THEN
               -- Sensor at front of train
               IF oldSensorState = open THEN             
                  -- Sensor now closed
                  -- First time sensor fired, front of train approaching sensor
                  -- Change state to closed
                  myPutLine("      -------------: front of train approaching sensor " & integer'image(sensorId) ); 
                 ELSE            
                  -- Sensor now open
                  -- Second time sensor fired, front of train leaving sensor
                  myPutLine("      -------------: front of train leaving sensor " & integer'image(sensorId) ); 
                  IF FirstSection.State = Reserved THEN
                     FirstSection.State := Occupied;
                     SendToOutQueue(makePutSectionStateMsg(FirstSection.Id, Occupied));
                     IF FirstSection.SensorList.Head.Sensor.Id = SensorPtr.Sensor.Id THEN
                        AddNewSensorToFront(FirstSection.TrainId, FirstSection.SensorList.Tail.Sensor);
                     ELSE
                        AddNewSensorToFront(FirstSection.TrainId, FirstSection.SensorList.Head.Sensor);
                     END IF;
                  ELSE
                     SecondSection.State := Occupied;
                     SendToOutQueue(makePutSectionStateMsg(SecondSection.Id, Occupied));
                     IF SecondSection.SensorList.Head.Sensor.Id = SensorPtr.Sensor.Id THEN
                        AddNewSensorToFront(SecondSection.TrainId, SecondSection.SensorList.Tail.Sensor);
                     ELSE
                        AddNewSensorToFront(SecondSection.TrainId, SecondSection.SensorList.Head.Sensor);
                     END IF;
                  END IF;
                  SendToTrainQueue(makeFrontSensorFiredMsg(FirstSection.TrainId), FirstSection.TrainId);
				  
                  declare
                     sensorsPtr : sensorArrayAccess;
                  begin
                     sensorsPtr := GetSensors(FirstSection.TrainId);
                     convertSensorArrayToList(sensorsPtr, sList); 
                     SendToOutQueue(makePutTrainPositionMsg(FirstSection.TrainId, sList)); 
                     makeEmpty(sList);
                     disposeSensorArray(sensorsPtr);
                  end;
				  
               END IF;
            ELSE
               -- Error: sensor not at front or back of any train
               myPutLine("      -------------: ERROR sensor not at front or back " & integer'image(sensorId) ); 
               SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
            END IF;
         ELSIF (FirstSection /= NULL OR SecondSection /= NULL) AND oldSensorState = Closed THEN
            -- Back of train leaving sensor 
            myPutLine("      -------------: back of train leaving sensor " & integer'image(sensorId) ); 
         ELSE
            -- Error
            myPutLine("    -------------: MYSTERY ERROR sensor not at front or back " & integer'image(sensorId) ); 
            SendToAllTrainQueues(makeSensorErrorMsg(SensorId));
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IdentifyTrain: " & Exception_Information(Error));
            myPutLine("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END IdentifyTrain;

      PROCEDURE MakeReservation (
            TrainId :        TrainIdType;
            Result  :    OUT Boolean) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
         Sensors    : SensorObjList;
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
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END MakeReservation;

      PROCEDURE MoveNextSwitch (
            TrainId : TrainIdType;
            State   : SwitchStateType) IS
         TrainPtr      : TrainObjPtr    := TrainList;
         SectionPtr    : SectionNodePtr;
         FrontSensorId : Positive;
         Result        : Boolean;
         switchPtr     : switchNodePtr;
      BEGIN
         WHILE TrainPtr /= NULL LOOP
            IF TrainPtr.TrainId = TrainId THEN
               GetSection(SectionPtr, TrainPtr.SensorList.Head.Sensor.Id, TrainPtr.SensorList.Head.Next.Sensor.Id);
               IF SectionPtr /= NULL THEN
                  FrontSensorId := TrainPtr.SensorList.Head.Sensor.Id;
                  LOOP
                     IF SectionPtr.Section.SensorList.Head.Sensor.Id = FrontSensorId THEN
                        SectionPtr := SectionPtr.Section.PrevSectionList.Head;
                     ELSE
                        SectionPtr := SectionPtr.Section.NextSectionList.Head;
                     END IF;
                     EXIT WHEN SectionPtr.Section.SwitchList.Head /= NULL;
                     IF SectionPtr.Section.SensorList.Head.Sensor.Id = FrontSensorId THEN
                        FrontSensorId := SectionPtr.Section.SensorList.Tail.Sensor.Id;
                     ELSE
                        FrontSensorId := SectionPtr.Section.SensorList.Head.Sensor.Id;
                     END IF;
                  END LOOP;
                  
                  switchPtr := SectionPtr.Section.SwitchList.Head;
                  if switchPtr.switch.state = state or else
                     (switchPtr.switch.state = beginThrown and state = thrown) or else
                     (switchPtr.switch.state = beginClosed and state = closed)        
                  then
                     return;
                  end if;
                                  
                  MoveSwitchPossible(SectionPtr.Section.SwitchList.Head, TrainId, Result);
                  IF Result THEN
                     IF State = Thrown THEN
                        SectionPtr.Section.SwitchList.Head.Switch.State := BeginThrown;
                     ELSE
                        SectionPtr.Section.SwitchList.Head.Switch.State := BeginClosed;
                     END IF;
                     SendLoseReservationMessages(SectionPtr.Section.SwitchList.Head);          -- mo 1/8/12
                     SendToOutQueue(makePutSwitchStateMsg(SectionPtr.Section.SwitchList.Head.Switch.Id, SectionPtr.Section.SwitchList.Head.Switch.State));
                     SendToOutQueue(makeSwReqMsg(SectionPtr.Section.SwitchList.Head.Switch.Id, State));
                  END IF;
               END IF;
               return;                   -- mo 1/31/12
            END IF;
            TrainPtr := TrainPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveNextSwitch: " & Exception_Information(Error));
            myPutLine("    trainId" & integer'image(trainId));
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
         
         MoveSwitchPossible(SwitchPtr, Result);
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
            myPutLine("    switchId" & integer'image(switchId));
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
         OutSectList : SectionObjList;
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
               BlockSections(SectionPtr.Section.BlockingList);
               SectionPtr := SectionPtr.Next;
            END LOOP;
            Result := True;
            return;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in PositionTrain: " & Exception_Information(Error));
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END PositionTrain;

      PROCEDURE ReleaseReservation (
            TrainId : TrainIdType) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved AND
                  SectionPtr.Section.TrainId = TrainId THEN
               SectionPtr.Section.State := Free;
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
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END ReleaseReservation;

      procedure removeFromTrainList(trainId : TrainIdType) is
         prev, curr      : trainObjPtr;
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
            myPutLine("    trainId" & integer'image(trainId));
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
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      end repositionTrain;

		procedure setAllSensorsOpen is 
			ptr : SensorNodePtr;
		begin
			ptr := sensorList.head;
			while ptr /= null loop
				ptr.sensor.state := open;
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
         ThrownSectionList : SectionObjList;
         ClosedSectionList : SectionObjList;
         ThrownUseable     : Boolean        := False;
         ClosedUseable     : Boolean        := False;
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
            myPutLine(" **************** EXCEPTION Layout pkg in SwitchFinishedMoving:" & Exception_Information(Error));
            myPutLine("    switchId" & integer'image(switchId));
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

      PROCEDURE NewSection (
            Id : Positive) IS
      BEGIN
         CurrentSection := NEW SectionObj;
         CurrentSection.Id := Id;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in NewSection: " & Exception_Information(Error));
            RAISE;
      END NewSection;

      PROCEDURE EndSection IS
      BEGIN
         IF SectionList.Head = NULL THEN
            SectionList.Head := NEW SectionNode;
            SectionList.Tail := SectionList.Head;
         ELSE
            SectionList.Tail.Next := NEW SectionNode;
            SectionList.Tail := SectionList.Tail.Next;
         END IF;
         SectionList.Tail.Section := NEW SectionObj;
         SectionList.Tail.Section.All := CurrentSection.All;
         Free_Section(CurrentSection);
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in EndSection: " & Exception_Information(Error));
            RAISE;
     END EndSection;

      -- Add a section to the end of a section list
      PROCEDURE AddToEnd (
            OutSectList : IN OUT SectionObjList;
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
            put_line("**************** EXCEPTION Layout pkg in AddToEnd: " & Exception_Information(Error));
            RAISE;
      END AddToEnd;

      -- check if a section is in a blocking list
      FUNCTION IsIn (
            BlockList : BlockingObjList;
            Id        : Positive)
        RETURN Boolean IS
         BlockingPtr : BlockingObjPtr := BlockList.Head;
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
            CurSectBlockList :        BlockingObjList;
            OutSectList      :    OUT SectionObjList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section /= NULL THEN
               IF SectionPtr.Section.Id /= CurrentSectId AND
                     NOT IsIn(CurSectBlockList, SectionPtr.Section.Id) AND
                     SectionPtr.Section.SensorList.Head /= NULL THEN
                  IF SectionPtr.Section.SensorList.Head.Sensor.Id =
                        SensorId OR
                        SectionPtr.Section.SensorList.Tail.Sensor.Id =
                        SensorId THEN
                     AddToEnd(OutSectList, SectionPtr);
                  END IF;
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
         OutSectionList : SectionObjList;
      BEGIN
         WHILE SectionPtr/= NULL LOOP
            FindAllSections(SectionPtr.Section.SensorList.Tail.Sensor.Id,
               SectionPtr.Section.Id, SectionPtr.Section.BlockingList,
               OutSectionList);
            SectionPtr.Section.NextSectionList := OutSectionList;
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
         OutSectionList : SectionObjList;
      BEGIN
         WHILE SectionPtr/= NULL LOOP
            FindAllSections(SectionPtr.Section.SensorList.Head.Sensor.Id,
               SectionPtr.Section.Id, SectionPtr.Section.BlockingList,
               OutSectionList);
            SectionPtr.Section.PrevSectionList := OutSectionList;
            OutSectionList.Head := NULL;
            OutSectionList.Tail := NULL;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in SetPrevSectionList: " & Exception_Information(Error));
            RAISE;
      END SetPrevSectionList;

      PROCEDURE EndSectionList IS
      BEGIN
         SetNextSectionList;
         SetPrevSectionList;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in EndSectionList: " & Exception_Information(Error));
            RAISE;
      END EndSectionList;

      FUNCTION CheckSensors (
            Sensors  : SensorObjList;
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

      PROCEDURE GetSections (
            SwitchPtr  :        SwitchObjPtr;
            ThrownList :    OUT SectionObjList;
            ClosedList :    OUT SectionObjList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
         SensorPtr  : SensorNodePtr;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            CASE SwitchPtr.TypeOfSwitch IS
               WHEN Normal =>
                  IF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SwitchPtr.ThrownSensor.Id) THEN
                     AddToEnd(ThrownList, SectionPtr);
                  ELSE
                     SensorPtr := SwitchPtr.ClosedSensors.Head;
                     WHILE SensorPtr /= NULL LOOP
                        IF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SensorPtr.Sensor.Id) THEN
                           AddToEnd(ClosedList, SectionPtr);
                           SensorPtr := NULL;
                        ELSE
                           SensorPtr := SensorPtr.Next;
                        END IF;
                     END LOOP;
                  END IF;
               WHEN Crossover =>
                  IF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SwitchPtr.NarrowSensors.Tail.Sensor.Id) THEN
                     AddToEnd(ThrownList, SectionPtr);
                  ELSIF CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Head.Sensor.Id, SwitchPtr.ClosedSensors.Head.Sensor.Id) OR
                        CheckSensors(SectionPtr.Section.SensorList, SwitchPtr.NarrowSensors.Tail.Sensor.Id, SwitchPtr.ClosedSensors.Tail.Sensor.Id) THEN
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
      PROCEDURE EndSwitchList IS
         SwitchPtr         : SwitchNodePtr  := SwitchList.Head;
         ClosedSectionList : SectionObjList;
         ThrownSectionList : SectionObjList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            GetSections(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
            IF SwitchPtr.Switch.State = Closed THEN
               SectionPtr := ThrownSectionList.Head;
               WHILE SectionPtr /= NULL LOOP
                  SectionPtr.Section.IsUseable := False;
                  SectionPtr := SectionPtr.Next;
               END LOOP;
            ELSE
               SectionPtr := ClosedSectionList.Head;
               WHILE SectionPtr /= NULL LOOP
                  SectionPtr.Section.IsUseable := False;
                  SectionPtr := SectionPtr.Next;
               END LOOP;
            END IF;
            
            -- The LocoBuffer has a 96 byte message buffer.
            -- Consequently, if messages are sent to it too rapidly then
            -- the buffer will overflow.
            -- Therefore we slow down sending messages when the LocoBufferServer is 
            -- running.
            if not simulator then
               delay 0.5;
            end if;
            
            SendToOutQueue(makeSwReqMsg(SwitchPtr.Switch.Id, SwitchPtr.Switch.State));
            if  SwitchPtr.Switch.State = Closed then
               SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, BeginClosed));
            else
               SendToOutQueue(makePutSwitchStateMsg(SwitchPtr.Switch.Id, BeginThrown));
            end if;
            ClosedSectionList.Head := NULL;
            ClosedSectionList.Tail := NULL;
            ThrownSectionList.Head := NULL;
            ThrownSectionList.Tail := NULL;
            SwitchPtr := SwitchPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in EndSwitchList: " & Exception_Information(Error));
            RAISE;
      END EndSwitchList;

      PROCEDURE FindSwitch (
            Switchs   :        SwitchObjList;
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
            put_line("**************** EXCEPTION Layout pkg in FindSwitch: " & Exception_Information(Error));
            myPutLine("    Looking for switch # " & Positive'Image(SwitchId));
            RAISE;
      END FindSwitch;

      PROCEDURE AddSwitch (
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
            SwitchPtr.Switch := NEW SwitchObj;
            SwitchPtr.Switch.Id := Id;
            SwitchList.Tail.Switch := SwitchPtr.Switch;
         ELSE
            CurrentSection.SwitchList.Tail.Switch := SwitchPtr.Switch;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddSwitch: " & Exception_Information(Error));
            myPutLine("    adding switch #" & Positive'Image(Id));
            RAISE;
      END AddSwitch;

      PROCEDURE UpdateSwitch (
            Id           : Positive;
            TypeOfSwitch : ControllerGlobals.SwitchType;
            state        : switchStateType) IS
         SwitchPtr : SwitchNodePtr;
      BEGIN
         FindSwitch(SwitchList, Id, SwitchPtr);
         IF SwitchPtr /= NULL THEN
            CurrentSwitch := SwitchPtr.Switch;
            CurrentSwitch.TypeOfSwitch := TypeOfSwitch;
            currentSwitch.state := state;
         ELSE
            RAISE InvalidSwitchId;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in UpdateSwitch: " & Exception_Information(Error));
            myPutLine("    switch #" & Positive'Image(Id));
            RAISE;
      END UpdateSwitch;

      PROCEDURE UpdateSwitchNarrow (
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
               CurrentSwitch.NarrowSensors.Tail.Sensor := SensorPtr.Sensor;
            ELSE
               RAISE InvalidSensorId;
            END IF;
         ELSE
            RAISE CurrentSwitchNull;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in UpdateSwitchNarrow: " & Exception_Information(Error));
            myPutLine("    narrow id #" & Positive'Image(narrowId));
            RAISE;
      END UpdateSwitchNarrow;

      PROCEDURE UpdateSwitchClosed (
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
               CurrentSwitch.ClosedSensors.Tail.Sensor := SensorPtr.Sensor;
            ELSE
               RAISE InvalidSensorId;
            END IF;
         ELSE
            RAISE CurrentSwitchNull;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in UpdateSwitchClosed: " & Exception_Information(Error));
            myPutLine("    closed id #" & Positive'Image(closedId));
            RAISE;
      END UpdateSwitchClosed;

      PROCEDURE UpdateSwitchThrown (
            ThrownId : Positive) IS
         SensorPtr : SensorNodePtr;
      BEGIN
         IF CurrentSwitch /= NULL THEN
            FindSensor(SensorList, ThrownId, SensorPtr);
            IF SensorPtr /= NULL THEN
               CASE CurrentSwitch.TypeOfSwitch IS
                  WHEN Normal =>
                     CurrentSwitch.ThrownSensor := SensorPtr.Sensor;
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
            put_line("**************** EXCEPTION Layout pkg in UpdateSwitchThrown: " & Exception_Information(Error));
            myPutLine("    thrown id #" & Positive'Image(thrownId));
            RAISE;
      END UpdateSwitchThrown;

      PROCEDURE AddSensor (Id : Positive) IS
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
            SensorPtr.Sensor := NEW SensorObj;
            SensorPtr.Sensor.Id := Id;
            SensorList.Tail.Sensor := SensorPtr.Sensor;
         ELSE
            CurrentSection.SensorList.Tail.Sensor := SensorPtr.Sensor;
         END IF;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddSensor: " & Exception_Information(Error));
            myPutLine("    sensor id #" & Positive'Image(Id));
            RAISE;
      END AddSensor;

      PROCEDURE AddBlocking (
            Id : Positive) IS
      BEGIN
         IF CurrentSection.BlockingList.Head = NULL THEN
            CurrentSection.BlockingList.Head := NEW BlockingObj;
            CurrentSection.BlockingList.Tail :=
               CurrentSection.BlockingList.Head;
         ELSE
            CurrentSection.BlockingList.Tail.Next := NEW BlockingObj;
            CurrentSection.BlockingList.Tail :=
               CurrentSection.BlockingList.Tail.Next;
         END IF;
         CurrentSection.BlockingList.Tail.Id := Id;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in AddBlocking: " & Exception_Information(Error));
            myPutLine("    id #" & Positive'Image(Id));
            RAISE;
      END AddBlocking;
		
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
            Sensors     : SensorObjList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False) IS
         SensorPtr : SensorNodePtr := Sensors.Head;
      BEGIN
         WHILE SensorPtr /= NULL LOOP
            Print("Sensor ID: " & Positive'Image(SensorPtr.Sensor.Id),
               Indent, Output);
            IF NOT PrintOnlyId THEN
               Print("Sensor State: " & SensorStateType'Image(
                     SensorPtr.Sensor.State), Indent, Output);
            END IF;
            SensorPtr := SensorPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("*************** EXCEPTION Layout pkg in PrintSensors: " & Exception_Information(Error));
            RAISE;
      END Print_Sensors;

      PROCEDURE Print_Switchs (
            Switchs     : SwitchObjList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False) IS
         SwitchPtr : SwitchNodePtr := Switchs.Head;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            Print("----------------------", Indent, Output);
            Print("Switch ID: " & Positive'Image(SwitchPtr.Switch.Id),
               Indent, Output);
            IF NOT PrintOnlyId THEN
               Print("Switch State: " & SwitchStateType'Image(SwitchPtr.Switch.State), Indent, Output);
               Print("Switch Type: " & ControllerGlobals.SwitchType'Image(SwitchPtr.Switch.TypeOfSwitch), Indent, Output);
               Print("Switch Narrow Sensors:", Indent, Output);
               Print_Sensors(SwitchPtr.Switch.NarrowSensors, Indent + 2, Output, True);
               Print("Switch Closed Sensors:", Indent, Output);
               Print_Sensors(SwitchPtr.Switch.ClosedSensors, Indent + 2, Output, True);
               IF SwitchPtr.Switch.ThrownSensor /= NULL THEN
                  Print("Switch Thrown Sensor: " & Positive'Image(SwitchPtr.Switch.ThrownSensor.Id), Indent, Output);
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
            BlockingList : BlockingObjList;
            Indent       : Natural;
            Output       : File_Type) IS
         BlockingPtr : BlockingObjPtr := BlockingList.Head;
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
            Sections    : SectionObjList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean        := False) IS
         SectionPtr : SectionNodePtr := Sections.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            if printOnlyId then
               Print("Section ID: " & Positive'Image(SectionPtr.Section.Id), Indent, Output);
            else
               Print("---------------", Indent, Output);
               Print("Section ID: " & Positive'Image(SectionPtr.Section.Id), Indent, Output);
               Print("Section State: " & SectionStateType'Image(
                     SectionPtr.Section.State), Indent, Output);
               Print("Section Is Useable: " & Boolean'Image(SectionPtr.Section.IsUseable), Indent, Output);
               IF SectionPtr.Section.State = Occupied OR SectionPtr.Section.State = Reserved THEN
                  Print("Section TrainAddr: " & TrainIdType'Image(SectionPtr.Section.TrainId), Indent, Output);
               END IF;
               IF SectionPtr.Section.State = Blocked THEN
                  Print("Section Block Count:" & Positive'Image(SectionPtr.Section.BlockCount), Indent, Output);
               END IF;
               Print("Section Sensors:", Indent, Output);
               Print_Sensors(SectionPtr.Section.SensorList, Indent + 2, Output, True);
               Print("Section Switchs:", Indent, Output);
               Print_Switchs(SectionPtr.Section.SwitchList, Indent + 2, Output, True);
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
            Sensors   :        SensorObjList;
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
            myPutLine("    Looking for sensor #" & Positive'Image(SensorId));
            RAISE;
      END FindSensor;

      PROCEDURE ReleaseBlockings (
            BlockingList : BlockingObjList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         IF BlockingList.Head /= NULL THEN
            WHILE SectionPtr /= NULL LOOP
               IF IsIn(BlockingList, SectionPtr.Section.Id) THEN
                  SectionPtr.Section.BlockCount := SectionPtr.Section.BlockCount - 1;
                  IF SectionPtr.Section.BlockCount = 0 THEN
                     SectionPtr.Section.State := Free;
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
		
      -- Add a new sensor to the front of a train's sensor list
      PROCEDURE AddNewSensorToFront (TrainId : TrainIdType; Sensor  : SensorObjPtr) IS
         TrainPtr  : TrainObjPtr   := TrainList;
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
            myPutLine("    train id #" & Positive'Image(trainId));
            RAISE;
      END AddNewSensorToFront;
         
      FUNCTION GetSensors (TrainId : TrainIdType) RETURN SensorArrayAccess IS
         TrainPtr  : TrainObjPtr       := TrainList;
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

      -- Get the two sections surrounding the sensor 
      PROCEDURE getSectionsContainingSensor (
            SensorID      :        Positive;
            FirstSection  :    OUT SectionObjPtr;
            SecondSection :    OUT SectionObjPtr) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         FirstSection := NULL;
         SecondSection := NULL;
         WHILE SectionPtr /= NULL LOOP
            IF (SectionPtr.Section.SensorList.Head.Sensor.Id = SensorId OR
                SectionPtr.Section.SensorList.Tail.Sensor.Id = SensorId) 
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
            put_line("**************** EXCEPTION Layout pkg in getSectionsContainingSensor: " & Exception_Information(Error));
            myPutLine("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END getSectionsContainingSensor;

      -- Get the two sections surrounding the sensor that
      --   are either occupied or reserved
      PROCEDURE GetOccResSections (
            SensorID      :        Positive;
            FirstSection  :    OUT SectionObjPtr;
            SecondSection :    OUT SectionObjPtr) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
      BEGIN
         FirstSection := NULL;
         SecondSection := NULL;
         WHILE SectionPtr /= NULL LOOP
            IF (SectionPtr.Section.SensorList.Head.Sensor.Id = SensorId OR
                  SectionPtr.Section.SensorList.Tail.Sensor.Id = SensorId) AND
                  (SectionPtr.Section.State = Occupied OR
                  SectionPtr.Section.State = Reserved) THEN
               IF FirstSection = NULL THEN
                  FirstSection := SectionPtr.Section;
               ELSIF FirstSection.TrainId = SectionPtr.Section.TrainId THEN
                  SecondSection := SectionPtr.Section;
                  RETURN;
               END IF;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in GetOccResSections: " & Exception_Information(Error));
            myPutLine("    sensor id #" & Positive'Image(sensorId));
            RAISE;
      END GetOccResSections;

      -- Remove the last sensor in a train's sensor list
      PROCEDURE RemoveLastSensor (
            TrainId : TrainIdType) IS
         TrainPtr  : TrainObjPtr   := TrainList;
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
            myPutLine("    train id #" & Positive'Image(trainId));
            RAISE;
      END RemoveLastSensor;

      -- Get a train's back sensor
      PROCEDURE GetBackSensor (TrainId : TrainIdType; BackId  : OUT Positive) IS
         TrainPtr : TrainObjPtr := TrainList;
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
            myPutLine("    train id #" & Positive'Image(trainId));
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
            myPutLine("    sensor id #" & Positive'Image(SensorPtr.Sensor.Id));
            RAISE;
		end flipSensor;

      -- Send a message to a specific TrainQueue
      PROCEDURE SendToTrainQueue (
            Cmd : MessageType;
            Id  : Positive) IS
         TrainPtr : TrainObjPtr := TrainList;
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
         TrainPtr : TrainObjPtr := TrainList;
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

      -- Check if it is possible to reserve this section bassed on switch states
      FUNCTION IsSectionUseable (SectionPtr : SectionObjPtr) RETURN Boolean IS
         SwitchPtr : SwitchNodePtr := SectionPtr.SwitchList.Head;
         Result    : Boolean;      -- := False;                         -- mo 1/6/12
         SensorPtr : SensorNodePtr;
         Id        : Positive;
      BEGIN
         WHILE SwitchPtr /= NULL LOOP
            result := false;                                             -- mo 1/6/12
            CASE SwitchPtr.Switch.TypeOfSwitch IS
               WHEN Normal =>
                  IF SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id THEN
                     Id := SectionPtr.SensorList.Tail.Sensor.Id;
                  ELSE
                     Id := SectionPtr.SensorList.Head.Sensor.Id;
                  END IF;
                  CASE SwitchPtr.Switch.State IS
                     WHEN Closed =>
                        SensorPtr := SwitchPtr.Switch.ClosedSensors.Head;
                        WHILE SensorPtr /= NULL LOOP
                           IF SensorPtr.Sensor.Id = Id THEN
                              Result := True;
                           END IF;
                           SensorPtr := SensorPtr.Next;
                        END LOOP;
                        IF NOT Result THEN
                           RETURN False;
                        END IF;
                     WHEN Thrown =>
                        IF Id /= SwitchPtr.Switch.ThrownSensor.Id THEN
                           RETURN False;
                        END IF;
                     WHEN OTHERS =>
                        RETURN False;
                  END CASE;
               WHEN Crossover =>
                  CASE SwitchPtr.Switch.State IS
                     WHEN Closed =>
                        IF NOT (SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Head.Sensor.Id) AND
                              NOT (SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Head.Sensor.Id) AND
                              NOT (SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id AND
                              SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Tail.Sensor.Id) AND
                              NOT (SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id AND
                              SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.ClosedSensors.Tail.Sensor.Id) THEN
                           RETURN False;
                        END IF;
                     WHEN Thrown =>
                        IF NOT (SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id) AND
                              NOT (SectionPtr.SensorList.Tail.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Head.Sensor.Id AND
                              SectionPtr.SensorList.Head.Sensor.Id = SwitchPtr.Switch.NarrowSensors.Tail.Sensor.Id) THEN
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
            SectList : SectionObjList)
        RETURN Boolean IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         IF SectionPtr = NULL THEN
            RETURN False;
         END IF;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State /= Free OR NOT IsSectionUseable(SectionPtr.Section) THEN
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
            myPutLine("    firstId" & integer'image(firstId) & " secondId" & integer'image(secondId));
            raise;
      END FindSection;

      -- Get the sections based on the list of sensors
      PROCEDURE FindAllSections (
            OutSectList :    OUT SectionObjList;
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
            SectList : SectionObjList;
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
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END PlaceTrainInSections;

      PROCEDURE BlockSections (BlockingList : BlockingObjList) IS
         SectionPtr : SectionNodePtr := SectionList.Head;
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
            myPutLine(" **************** EXCEPTION Layout pkg in BlockSections:" & Exception_Information(Error));
            raise;
      END BlockSections;

      PROCEDURE GetSensor (
            SensorId :        Positive;
            Sensor   :    OUT SensorObjPtr) IS
         SensorPtr : SensorNodePtr := SensorList.Head;
      BEGIN
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
            myPutLine("    sensorId" & integer'image(sensorId));
            raise;
      END GetSensor;

      -- mo 1/18/12
      function isNewTrain(TrainId : TrainIdType) return boolean is
         TrainPtr  : TrainObjPtr := trainList;
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
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      end isNewTrain;
  
      
      FUNCTION IsIn (
            Sections : SectionObjList;
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
            myPutLine("    Id" & integer'image(Id));
            raise;
      END IsIn;


      PROCEDURE GetFreeSection (
            SectList   :        SectionObjList;
            OutSectPtr :    OUT SectionObjPtr) IS
         SectionPtr : SectionNodePtr := SectList.Head;
      BEGIN
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Free AND
                  IsSectionUseable(SectionPtr.Section) THEN
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
            Sensors :    OUT SensorObjList) IS
         TrainPtr : TrainObjPtr := TrainList;
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
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END GetTrainSensorList;


      -- PROCEDURE SetNotUseable (
            -- SwitchPtr : SwitchNodePtr) IS
         -- ThrownSectionList : SectionObjList;
         -- ClosedSectionList : SectionObjList;
         -- SectionPtr        : SectionNodePtr;
      -- BEGIN
         -- GetSections(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         -- SectionPtr := ThrownSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- SectionPtr.Section.IsUseable := False;
            -- SectionPtr := SectionPtr.Next;
         -- END LOOP;
         -- SectionPtr := ClosedSectionList.Head;
         -- WHILE SectionPtr /= NULL LOOP
            -- SectionPtr.Section.IsUseable := False;
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
            myPutLine("    frontSensorId" & integer'image(frontSensorId) & " backSensorId" & integer'image(backSensorId));
            raise;
      END GetSection;

      -- Is it possible for a train to move this switch
      PROCEDURE MoveSwitchPossible (
            SwitchPtr :        SwitchNodePtr;
            TrainId   :        TrainIdType;
            Result    :    OUT Boolean) IS
         ThrownSectionList : SectionObjList;
         ClosedSectionList : SectionObjList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         GetSections(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied OR
                  (SectionPtr.Section.State = Reserved AND
                  SectionPtr.Section.TrainId /= TrainId) THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied OR
                  (SectionPtr.Section.State = Reserved AND
                  SectionPtr.Section.TrainId /= TrainId) THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         Result := True;
      EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in MoveSwitchPossible: " & Exception_Information(Error));
            myPutLine("    trainId" & integer'image(trainId));
            raise;
      END MoveSwitchPossible;

      -- Is it possible to move this switch
      PROCEDURE MoveSwitchPossible (
            SwitchPtr :        SwitchNodePtr;
            Result    :    OUT Boolean) IS
         ThrownSectionList : SectionObjList;
         ClosedSectionList : SectionObjList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         GetSections(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied THEN
               Result := False;
               RETURN;
            END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Occupied THEN
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
         ThrownSectionList : SectionObjList;
         ClosedSectionList : SectionObjList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         thereIsReservation := false;
         trainId := trainIdType'first;
         GetSections(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved THEN
               thereIsReservation := true;
               trainId := SectionPtr.Section.TrainId;
               return;
               END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved THEN
               thereIsReservation := true;
               trainId := SectionPtr.Section.TrainId;
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
         ThrownSectionList : SectionObjList;
         ClosedSectionList : SectionObjList;
         SectionPtr        : SectionNodePtr;
      BEGIN
         GetSections(SwitchPtr.Switch, ThrownSectionList, ClosedSectionList);
         SectionPtr := ThrownSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved THEN
               SendToTrainQueue(makeLoseReservationMsg(SectionPtr.Section.TrainId), SectionPtr.Section.TrainId);
               return;
               END IF;
            SectionPtr := SectionPtr.Next;
         END LOOP;
         SectionPtr := ClosedSectionList.Head;
         WHILE SectionPtr /= NULL LOOP
            IF SectionPtr.Section.State = Reserved THEN
               SendToTrainQueue(makeLoseReservationMsg(SectionPtr.Section.TrainId), SectionPtr.Section.TrainId);
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
					
		procedure makeEmptySensorObjList(sol : in out sensorObjList) is 
			cur, temp  : sensorNodePtr;
		begin
			cur := sol.head;
			while cur /= null loop
				temp := cur;
				cur := cur.next;
				disposeSensorNode(temp);
			end loop;
		end makeEmptySensorObjList;
		
		-- procedure disposeTrainObj(ptr : in out TrainObjPtr) is
		-- begin
			-- if ptr /= null then
				-- makeEmptySensorObjList(ptr.sensorList);
				-- disposeBasicTrainObj(ptr);
			-- end if;
		-- end disposeTrainObj;

		procedure updateTrainSensors(TrainId : TrainIdType; Sensors : SensorArrayType) IS
		-- pre TrainId is in TrainList
			TrainPtr  : TrainObjPtr := trainList;
			SensorPtr : SensorObjPtr;
		begin
		
			-- Find the train
			while trainPtr.trainId /= trainId loop
				trainPtr := trainPtr.next;
			end loop;
			
			-- Remove old sensors
			makeEmptySensorObjList(trainPtr.sensorList);              
			
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
				myPutLine("    trainId" & integer'image(trainId));
				raise;
		 end updateTrainSensors;
	 
		PROCEDURE AddNewTrain (TrainId : TrainIdType; Sensors : SensorArrayType) IS
			TrainPtr  : TrainObjPtr;
			SensorPtr : SensorObjPtr;
		BEGIN
			IF TrainList = NULL THEN
				TrainList := NEW TrainObj;
				TrainList.TrainId := TrainId;
				CommandQueueManager.TrainIdQueueList.GetQueue(TrainId, TrainList.Queue);
				TrainPtr := TrainList;
			ELSE
				TrainPtr := TrainList;
				WHILE TrainPtr.Next /= NULL LOOP
					TrainPtr := TrainPtr.Next;
				END LOOP;
				TrainPtr.Next := NEW TrainObj;
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
				myPutLine("    trainId" & integer'image(trainId));
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
            -- Get Messages from the LayoutQueue
            -- IF false THEN    
               -- DELAY 0.01; -- Wait so that other tasks have a chance to run    test 1
            -- else
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
                           null;
                        else                        
                           LayoutPtr.IdentifyTrain(SensorId);               -- mo 1/20/12                          
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
