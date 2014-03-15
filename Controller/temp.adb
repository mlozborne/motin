      procedure errorStopTrainsAtBadSensor(idSensorCase         : natural;
		                                     sensorId             : positive;
														 leftSectionPtr       : sectionObjPtr;
														 rightSectionPtr      : sectionObjPtr) is
		begin
			myPutLine("      -------------IdentifyTrainV3: EXPECTATION ERROR at sensor " & 
			          integer'image(sensorId) & 
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
				sendToTrainQueue(makeSensorErrorMsg(SensorId), train1);
			end if;
			if train2 /= 0 and train2 /= train1 then
				myPutLine("                                    stopping train " & integer'image(train2));
				sendToTrainQueue(makeSensorErrorMsg(SensorId), train2);
			end if;
		end if;

		
		PROCEDURE IdentifyTrainV3 (SensorID : Positive) IS
		begin
			-- Determine if sensor is in the layout specification even if it exists physical.
         FindSensor(SensorList, SensorID, SensorPtr);
         IF SensorPtr = NULL THEN
            myPutLine("      -------------IdentifyTrainV1: ERROR(maybe) sensor not recognized " & integer'image(sensorId) ); 
            return;
         end if;
			
			-- Remember the sensor's state:  oldSensorState
			-- Flip the sensor:  newSensorState
			-- Display the old sensor state
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
					errorStopTrainsAtBadSensor(idSensorCase, sensorId, leftSectionPtr, rightSectionPtr);
				else
					if oldSensorState = closed then
						myPutLine("      -------------IdentifyTrainV1: C2 NORMAL back of train leaving closed sensor sn"); 
					else
						myPutLine("      -------------IdentifyTrainV1: C2 SENSOR ERROR back of train leaving open sensor. Stop train.");
						sendToTrainQueue(makeSensorErrorMsg(SensorId), trainId);	
				end if;
				
			elsif idSensorCase = 3 then
			
				-- Case 3: sx=si in <s2,...,sn-1>, where n  > 2
				if expectationError then
					errorStopTrainsAtBadSensor(idSensorCase, sensorId, leftSectionPtr, rightSectionPtr);
				else
					SensorPtrs := GetSensorPtrs(TrainId); -- This array contains pointers to s1,s2,...,sn
					last := sensorPtrs'last;
					
					if oldSensorState = closed then
						myPutLine("      -------------IdentifyTrainV1: C3 FIXING sensor unexpectedly closed. Flip and continue"); 
						flipSensor(sensorPtr);
						SendToOutQueue(makePutSensorStateMsg(SensorId, closed));
					end if;
					-- NORMAL from here if sn-1 fired else fixing by removing extra sensorStateType
					for i in reverse 2..last loop
						exit when sensorId = sensorPtrs(i).id;
						myPutLine("      -------------IdentifyTrainV1: C3 removing sensor"  
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
					errorStopTrainsAtBadSensor(idSensorCase, sensorId, leftSectionPtr, rightSectionPtr);
				else

				
		EXCEPTION
         WHEN Error : OTHERS =>
            put_line("**************** EXCEPTION Layout pkg in IdentifyTrainV3: " & Exception_Information(Error));
            raise;
		end IdentifyTrainV3;

		
	   /sensorid/sx/
		/trainv1/TrainV3/
		