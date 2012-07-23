with ada.exceptions; use ada.exceptions;
with ada.text_io; use ada.text_io;

with screenManager; use screenManager;
with TcpIp; use TcpIp;
with logFiles;
with messageTranslationLibrary; use messageTranslationLibrary;


package body RailroadManager is

   PROTECTED body RailroadManagerType IS
   
      function toString(switches : switchArrayType) return string is
         str : string(switches'range);
      begin
         for i in switches'range loop
            case switches(i) is 
               when closed =>
                  str(i) := 'C';
               when thrown =>
                  str(i) := 'T';
               when beginThrown => 
                  str(i) := 't';
					when beginClosed =>
                  str(i) := 'c';
               when unknown | read =>
                  str(i) := '?';
            end case;
         end loop;
         return str;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.toString(switches) --" & kLFString & Exception_Information (error));
            raise;
      end toString;

      procedure putTrains(trains : trainArrayType) is 
         TheDirection : Character;
         TheLight, TheBell, TheHorn, TheMute : String(1..3);
      begin
         for i in trains'range loop
            IF Trains(I).Direction = Forward THEN TheDirection := 'F';
            ELSE TheDirection := 'R';
            END IF;

            IF (Trains(I).Light = On) THEN TheLight := "on ";
            ELSE TheLight := "off";
            END IF;

            IF (Trains(I).Bell = On) THEN TheBell := "on ";
            ELSE TheBell := "off";
            END IF;

            IF (Trains(I).Horn = On) THEN TheHorn := "on ";
            ELSE TheHorn := "off";
            END IF;

            IF (Trains(I).Mute= On) THEN TheMute := "on ";
            ELSE TheMute := "off";
            END IF;   
            objScreenManager.putTrain(
               i,
               trains(i).phyadr, trains(i).physlot,
               trains(i).viradr, trains(i).virslot,
               trains(i).state,
               trains(i).speed,
               theDirection,
               theLight,
               theBell,
               theHorn,
               theMute,
               trains(i).sensors);
         end loop;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManagerType.putTrains --" & kLFString & Exception_Information (error));
            raise;
      end putTrains;
      
      function getIdOfTrainFromPhyAdr(pa : natural) return natural is
      begin
         for i in trains'range loop
            if trains(i).phyAdr = pa then 
               return i;
            end if;
         end loop;
         return 0;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.getIdOfTrainFromPhyAdr --" & kLFString & Exception_Information (error));
            raise;
      end getIdOfTrainFromPhyAdr;

      procedure completeEntryForTrainWith(physAdd : natural; physslot : slotType; virtAdd : natural; virtSlot : natural) is
         id : natural;
      begin
         id := getIdOfTrainFromPhyAdr(physAdd);
         trains(id).isConnected := true;
         trains(id).sensorsOK := true;
         trains(id).phyAdr := physAdd;
         trains(id).phySlot := physSlot;
         trains(id).virAdr := virtAdd;
         trains(id).virSlot := virtSlot;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.completeEntryForTrainWith --" & kLFString & Exception_Information (error));
            raise;
      end completeEntryForTrainWith;
   
      function trainIdIsValid(n : positive) return boolean is
      begin
         return trains(n).isConnected and trains(n).sensorsOK;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.trainIdIsValid --" & kLFString & Exception_Information (error));
            raise;
      end trainIdIsValid;
   
      procedure reinitializeTrain(id : natural) is
      begin
         trains(id).sensorsOK := false;
         trains(id).state := "  ";
         trains(id).direction := forward;
         trains(id).speed := 0;
         trains(id).light := off;
         trains(id).horn := off;
         trains(id).bell := off;
         trains(id).mute := off;
         makeEmpty(trains(id).sensors);                -- memory leak
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.reinitializeTrain --" & kLFString & Exception_Information (error));
            raise;
      end reinitializeTrain;
         

      function trainTableIsFull return boolean is
      begin
         return numTrainsInUse >= kNumTrains;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.trainTableIsFull --" & kLFString & Exception_Information (error));
            raise;
      end trainTableIsFull;
      
      procedure addTrainUsingPhyAdr(pa : natural) is
      begin
         for i in Trains'range loop
            if trains(I).phyAdr = 0 then
               trains(i).phyAdr := pa;
               numTrainsInUse := numTrainsInUse + 1;
               return;
            end if;
         end loop;
         raise TrainsTableFull;
      exception
         when trainsTableFull =>
            raise;
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.addTrainUsingPhyAdr --" & kLFString & Exception_Information (error));
            raise;
      end addTrainUsingPhyAdr;
                             
      procedure removeAllTrains is
         nullTrain : trainType;
      begin
         NumTrainsInUse := 0;
         for i in trains'range loop
            trains(i) := nullTrain;
         end loop;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.removeAllTrains --" & kLFString & Exception_Information (error));
            raise;
      end removeAllTrains;

      procedure removeTrainUsingPhyAdr(pa : natural) is
         nullTrain : trainType;
      begin
         for i in trains'range loop
            if trains(i).phyAdr = pa then 
               trains(i) := nullTrain;
               numTrainsInUse := numTrainsInUse - 1;
               return;
            end if;
         end loop;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.removeTrainUsingPhyAdr --" & kLFString & Exception_Information (error));
            raise;
      end removeTrainUsingPhyAdr;

      function getIdOfTrainFromVSlot(vs : natural) return natural is
      begin
         for i in trains'range loop
            if trains(i).virslot = vs then 
               return i;
            end if;
         end loop;
         return 0;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.getIdOfTrainFromVSlot --" & kLFString & Exception_Information (error));
            raise;
      end getIdOfTrainFromVSlot;

      function physicalLocoAddressAlreadyPresent(pa : natural) return boolean is
      begin
         for i in trains'range loop
            if trains(i).PhyAdr = pa then
               return true;
            end if;
         end loop;
         return false;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.physicalLocoAddressAlreadyPresent --" & kLFString & Exception_Information (error));
            raise;
      end physicalLocoAddressAlreadyPresent;

      function toString2(trainState : trainStateType) return string is
      begin
         case trainState is
            when moving =>
               return "M ";
            when waiting =>
               return "W ";
            when halted =>
               return "H ";
            when error =>
               return "E ";
            when beginChangeDirection =>
               return "BD";
            when beginWaiting =>
               return "BW";
            when beginHalted =>
               return "BH";
            when others =>
               return "XX";
         end case;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.toString2 --" & kLFString & Exception_Information (error));
            raise;
      end toString2;

      procedure Initialize is
      begin
         putTrains(trains);
         ObjScreenManager.PutSwitches(toString(switches));
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.Initialize --" & kLFString & Exception_Information (error));
            raise;
     end Initialize;

      PROCEDURE Put(message : messageType) is
      -- This processes messages received through the TCP/IP connection
         addressReceived            : natural := 0;
         addressIsAlreadyInUse      : boolean := false;
         slotNumberReceived         : natural := 0;
         command                    : commandType;
         switchNum                  : natural := 0;
         switchSetting              : SwitchStateType;
         trainDirection             : directionType;
         speed                      : natural;
         light, horn, bell, mute    : OnOffType;
         virSlotNum, trainId        : natural;
         trainState                 : trainStateType;
         location                   : natural := 0;

         physSlot, virtslot         : slotType;
			physAdd, virtAdd           : locoAddressType;
         sensors                    : naturalListType;
      begin
        objScreenManager.putMessage(toString(message), "Received "& toEnglish(message));
		
        case message.byteArray(1) is

            when OPC_GPON =>
			
			   null;

            when OPC_GPOFF =>
			
               null;

            when OPC_INPUT_REP =>

					null;

            when OPC_SW_REP =>

               splitSwRepMsg(message, switchNum, switchSetting);
               if switchNum <= kNumSwitches then
				   switches(switchNum) := switchSetting;
				   objScreenManager.putSwitches(toString(switches));
               else
                   objScreenManager.putError("OPC_SW_REP: Switch number" & integer'image(switchNum) & " out of range");
			   end if;
			   
            when OPC_LOCO_ADR =>

					null;

            when OPC_SL_RD_DATA =>

               splitSlRdDataMsg(message, addressReceived, addressIsAlreadyInUse, slotNumberReceived);
               if (addressReceived /= addressBeingSet) then

                  objScreenManager.putError("This message is a response to another client");

               else

                  if (tryingToSteal) then
                    -- Responding to a steal
                    tryingToSteal := false;
                    if (not addressIsAlreadyInUse) then
                      -- Error, steal failed because loco address not yet in-use
                      objScreenManager.putError("ERROR: Steal failed because address not yet in-use");
                    else
                      -- Steal successful
                      -- Record train address and slot number                     
                      addTrainUsingPhyAdr(addressBeingSet);
                      completeEntryForTrainWith(addressBeingSet, slotNumberReceived, addressBeingSet, slotNumberReceived);
                      objScreenManager.putError("Successful steal");
                      putTrains(trains);         
                    end if;
                  end if;

                  if (tryingToSelect) then
                    -- Responding to a select
                    tryingToSelect := false;
                    if (addressIsAlreadyInUse) then
                     -- Error, select failed because loco address already in-use
                     objScreenManager.putError("Select failed because address already in-use");
                    else
                      -- Select successful.
                      -- Record train address and slot number.
                      -- Send OPC_MOVE_SLOTS to set the slot to in-use                      
                      addTrainUsingPhyAdr(addressBeingSet);
                      completeEntryForTrainWith(addressBeingSet, slotNumberReceived, addressBeingSet, slotNumberReceived);
                      objScreenManager.putError("Successful select");
                      putTrains(trains);                       

                      command.cmd := MoveSlots;
                      command.n := slotNumberReceived;
                      sendMessage(command);
                    end if;
                  end if;
                  putTrains(trains);
               end if;

            when OPC_LONG_ACK =>

					null;

            when OPC_MOVE_SLOTS =>

					null;

            when OPC_LOCO_SPD =>

					null;

            when OPC_LOCO_DIRF =>
   
               null;

            when OPC_LOCO_SND =>

					null;

            when OPC_SW_REQ =>

               null;

            when 16#00# =>

               -- These are the extended instructions
               case message.byteArray(2) is
					
						when putPowerChangeComplete =>
						
							objScreenManager.putError("Power change completed");

                  when putTrainState =>

                     splitPutTrainStateMsg(message, virSlotNum, trainState);
                     trainId := getIdOfTrainFromVSlot(virSlotNum);
                     if trainId = 0 then
                        objScreenManager.putError("putTrainState for an address that is not yet in-use");
                        return;
                     end if;
                     trains(trainId).state := toString2(trainState);
                     objScreenManager.putError("putTrainState processed");
                     putTrains(trains);

                  when putTrainPosition =>

                     splitPutTrainPositionMsg(message, virSlotNum, sensors);
                     trainId := getIdOfTrainFromVSlot(virSlotNum);
                     if trainId = 0 then
                        objScreenManager.putError("putTrainPosition for an address that is not yet in-use");
								makeEmpty(sensors);
                        return;
                     end if;
                     copyFromTo(sensors, trains(trainId).sensors);
							makeEmpty(sensors);
                     objScreenManager.putError("putTrainPosition processed");
                     putTrains(trains);

                  when putSectionState =>
				  
							null;

                  when putSwitchState =>

                     splitPutSwitchStateMsg(message, switchNum, switchSetting);
                     switches(switchNum) := switchSetting;
                     objScreenManager.putError("putSwitchState processed");
                     objScreenManager.putSwitches(toString(switches));

                  when putSensorState =>

							-- Tell the screen manager.
							declare
								sensorId      : positive;
								sensorState   : sensorStateType;
							begin
								splitPutSensorStateMsg(message, sensorId, sensorState);
								objScreenManager.putSensor(sensorId, sensorState);
							end;
							
                  when putInitOutcome =>

                     -- This is assumed to be a response to a prior doLocoInit.
                     splitPutInitOutcomeMsg(message, physAdd, physSlot, virtAdd, virtSlot);

                     if physSlot = 121 then
                        objScreenManager.putError("ERROR: invalid sensor numbers in response to Z...");
                     elsif physSlot = 122 then
                        objScreenManager.putError("ERROR: insufficient slots in DCS200 in response to Z...");
                     elsif physSlot = 123 then
                        objScreenManager.putError("ERROR: try again after previous initialization completes in response to Z...");
                     elsif physSlot = 124 then
                        objScreenManager.putError("ERROR: train's position conflicts with another train's in response to Z...");
                     elsif physSlot > 124 then
                        objScreenManager.putError("ERROR: unexpected response to command Z...");
                     else
                        completeEntryForTrainWith(physAdd, physSlot, virtAdd, virtSlot);
                        objScreenManager.putError("Successful initialization");
                        putTrains(trains);                       
                     end if;

                  when putReadLayoutResponse =>

							null;

                  when putTrainInformation =>

                     splitPutTrainInformationMsg(message, virSlotNum, speed, trainDirection, light, bell, horn, mute);
                     trainId := getIdOfTrainFromVSlot(virSlotNum);
                     if trainId = 0 then
                        objScreenManager.putError("putTrainInformation for an address that is not yet in-use");
                        return;
                     end if;
                     trains(trainId).speed := speed;
                     trains(trainId).direction := trainDirection;
                     trains(trainId).light := light;
                     trains(trainId).horn := horn;
                     trains(trainId).bell := bell;
                     trains(trainId).mute := mute;
                     objScreenManager.putError("putTrainInformation processed");
                     putTrains(trains);

                  when others =>

							null;

                  end case;

            when others =>

					null;

         end case;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.Put --" & kLFString & Exception_Information (error));
            raise;
      end Put;

      procedure sendMessage(command : in out commandType) is
		-- This process messages to be sent over through the TCP/IP connection

         function toggle(flag : OnOffType) return OnOffType is
         begin
            if flag = On then
               return Off;
            else
               return On;
            end if;
         exception
            when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.toggle --" & kLFString & Exception_Information (error));
			   raise;
         end toggle;

         message           : messageType;
         cmd               : cmdType := command.cmd;
         n                 : natural := command.n;
         size              : integer := -1;
         switchDirection   : switchStateType;
         location          : natural := 0;

      begin

         case cmd is

            when PowerOn =>

               message := makePowerOnMsg;
               logFiles.openAdminLog;

            when PowerOff =>

               message := makePowerOffMsg;
               logFiles.closeAdminLog;

            when MoveSlots =>

               message := makeMoveSlotsMsg(n, n);

            when SelectLoco | StealLoco =>

               addressBeingSet := n;
               if getIdOfTrainFromPhyAdr(addressBeingSet) /= 0 then
                  objScreenManager.putError("ERROR: You have already listed this physical loco address.");
                  return;
               end if;

               if trainTableIsFull then
                  objScreenManager.putError("ERROR: You have already set the maximum number of trains.");
                  return;
               end if;

               if cmd = SelectLoco then
                  tryingToSelect := true;
                  objScreenManager.putError("Attempting to select a physical loco address.");
               else
                  tryingToSteal := true;
                  objScreenManager.putError("Attempting to steal a physical loco address.");
               end if;
               message := makeLocoAdrMsg(addressBeingSet);

            when RemoveLoco =>
            
                  -- This is a request to remove one train.
                  -- Find out where the train is stored in the trains array and remove it.
                  -- Send the message to the railroad if the train was in the trains array.
                  location := getIdOfTrainFromPhyAdr(command.n);
                  IF Location = 0 THEN
                     -- The train is not in the array
                     objScreenManager.putError
                     ("Failure: you haven't established this train's physical address");
                     return;
                  ELSE
                     -- The train is in the array, so get rid of it
                     if not trains(location).isConnected then
                        objScreenManager.putError
                        ("Failure: this train was never registered successfully by this throttle");
                        return;
                     else
                        objScreenManager.putError("Success: message sent to the controller");
                     end if;
                  END IF;
                  
                  -- Stop the train
                  message := makeLocoSpdMsg(trains(location).phySlot, 1);
                  size := sendTCPMessage(message);

                  -- Clear the entry in the slot lookup table
                  message := makeWriteSlotDataToClearMsg(trains(location).phySlot);
                  removeTrainUsingPhyAdr(command.n);
                  PutTrains(Trains);
            
            when Forward | Backward | Horn | Bell | Light =>
            
               if not trainIdIsValid(n) then
                  objScreenManager.putError("ERROR: A train with this id has NOT been established properly");
                  return;
               else
                  case cmd is
                     when Forward  => trains(n).direction := Forward;
                     when Backward => trains(n).direction := Backward;
                     when Horn     => trains(n).Horn  := toggle(trains(n).Horn);
                     when Bell     => trains(n).Bell  := toggle(trains(n).Bell);
                     when Light    => trains(n).Light := toggle(trains(n).Light);
                     when others   => null;
                  end case;
                  message := makeLocoDirfMsg(trains(n).virslot, trains(n).direction, trains(n).light, trains(n).horn, trains(n).bell);
                  PutTrains(Trains);
               end if;

            when Mute =>

               if not trainIdIsValid(n) then
                  objScreenManager.putError("ERROR: A train with this id has NOT been established properly");
                  return;
               else
                  trains(n).Mute := toggle(trains(n).Mute);
                  message := makeLocoSndMsg(trains(n).virslot, trains(n).mute);
                  PutTrains(Trains);
               end if;

            when Speed =>

               if not trainIdIsValid(n) then
                  objScreenManager.putError("ERROR: A train with this id has NOT been established properly");
                  return;
               else
                  trains(n).speed := command.speed;
                  message := makeLocoSpdMsg(trains(n).virslot, trains(n).speed);
                  PutTrains(Trains);
               end if;

            when Halt =>

               for i in trains'range loop
                  trains(i).speed := 0;
               end loop;
               putTrains(trains);

            when Close | Throw =>

               case cmd is
                  when Close  =>
                     switches(n) := beginClosed;
                     switchDirection := closed;
                  when Throw  =>
                     switches(n) := beginThrown;
                     switchDirection := thrown;
                  when others =>
                     switchDirection := unknown;
               end case;
               message := makeSwReqMsg(n, switchDirection);
               ObjScreenManager.PutSwitches(toString(switches));

            when ReadXML =>

               message := makeDoReadLayoutMsg(command.fileName(1..command.fnInUse));

            when InitializeLoco =>

					objScreenManager.makeEmptyClosedSensorList;
					if getCount(command.sensors) = 1 and then getCurrent(moveFront(command.sensors)) = 0 then
					   -- if the sensor list contains the single entry 0, then retain the
						-- current sensor list for the train. Thus the train will be reinitialized using
						-- it current sensor list. This depends on the train being present in the throttle.
						location := getIdOfTrainFromPhyAdr(command.n);
                  IF Location /= 0 THEN
							if not isEmpty(trains(location).sensors) then
								copyFromTo(trains(location).sensors, command.sensors);
							end if;
						end if;
					end if;
               message := makeDoLocoInitMsg(Command.N, Command.Sensors);
					
               IF Command.N = 9999 THEN
                  -- This is a request to remove all trains
                  removeAllTrains;
                  PutTrains(Trains);
               ELSIF getCount(command.sensors) = 0 THEN
                  -- This is a request to remove one train, which may or may not have been established
                  -- by this admin throttle.
                  -- Find out where the train is stored in the trains array and remove it.
                  -- Send the message to the controller no matter what.
                  location := getIdOfTrainFromPhyAdr(command.n);
                  IF Location = 0 THEN
                     -- The train is not in the array
                     objScreenManager.putError
                     ("Success: you haven't established this train's physical address but message sent to controller");
                  ELSE
                     -- The train is in the array, so get rid of it
                     if not trains(location).isConnected then
                        objScreenManager.putError
                        ("Success: this train was never initialized successfully but message sent to controller");
                     else
                        objScreenManager.putError
                        ("Success: message sent to the controller");
                     end if;
                  END IF;
                  removeTrainUsingPhyAdr(command.n);
                  PutTrains(Trains);
               else
                  -- This is a request to initialize or reinitialize a train
                  -- See if the train is already in the Trains table
                  location := getIdOfTrainFromPhyAdr(command.n);
                  IF Location = 0 THEN
                     -- This is a request to initialize a new train
                     if trainTableIsFull then
                        -- The table is full, so don't send the message
                        objScreenManager.putError("ERROR: Max num trains already present");
                        return;
                     else
                        -- Add this train to the table by initializing the phyAddr field
                        addTrainUsingPhyAdr(command.n);
                        putTrains(trains);
                     end if;
                  else
                     -- This is a request to reinitialize an existing train
                     reinitializeTrain(location);
                     putTrains(trains);
                  end if;                         
               end if;

            when others =>

               objScreenManager.putError("ERROR: Unrecongized message");
               return;

         end case;

         if cmd /= halt then
            size := sendTCPMessage(message);
            objScreenManager.putError("sending " & toEnglish(message));
         end if;

         if cmd = halt then
            for i in trains'range loop
               if trainIdIsValid(i) then
                  message := makeLocoSpdMsg(trains(i).virslot, 0);
                  size := sendTCPMessage(message);
                  objScreenManager.putError("sending " & toEnglish(message));
               end if;
	        end loop;
         end if;

      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.sendMessage --" & kLFString & Exception_Information (error));
            raise;
      end sendMessage;

   END RailroadManagerType;

end RailroadManager;