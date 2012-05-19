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
               when beginThrown | beginClosed =>
                  str(i) := '*';
               when unknown =>
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
      -- This shows messages received
         addressReceived            : natural := 0;
         addressIsAlreadyInUse      : boolean := false;
         slotNumberReceived         : natural := 0;
         slotNum                    : natural;
         command                    : commandType;
         sensorNum                  : natural;
         switchNum                  : natural := 0;
         switchSetting              : SwitchStateType;
         switchStr                  : string(1..6);
         trainDirection             : directionType;
         speed                      : natural;
         light, horn, bell, mute,
         F5, F6            			: onOffType;
         virSlotNum, trainId        : natural;
         trainState                 : trainStateType;
         isHi                       : boolean;
         location                   : natural := 0;

         physSlot, virtslot         : slotType;
			physAdd, virtAdd           : locoAddressType;
         sensors                    : naturalListType;

		 ii : integer := 0;
		 jj : integer := 0;
      begin
         case message.byteArray(1) is

             when OPC_GPON =>
               objScreenManager.putMessage(toString(message), "Another throttle is turning the power on");

             when OPC_GPOFF =>

               objScreenManager.putMessage(toString(message), "Another throttle is turning the power off");

            when OPC_INPUT_REP =>

               splitInputRepMsg(message, sensorNum, isHi);
               if isHi then
                  objScreenManager.putMessage(toString(message) , "Railroad is reporting sensor " & natural'image(sensorNum) & " fired hi");
               else
                  objScreenManager.putMessage(toString(message) , "Railroad is reporting sensor " & natural'image(sensorNum) & " fired low");
               end if;

            when OPC_SW_REP =>

               splitSwRepMsg(message, switchNum, switchSetting);
               switchStr := toString(switchSetting);
               objScreenManager.putMessage(toString(message), "Railroad is reporting switch " & natural'image(switchNum)& " " & switchStr);
               switches(switchNum) := switchSetting;
               objScreenManager.putSwitches(toString(switches));

            when OPC_LOCO_ADR =>

               objScreenManager.putMessage(toString(message), "Another throttle is requesting address info with OPC_LOCO_ADR");

            when OPC_SL_RD_DATA =>

               objScreenManager.putMessage(toString(message), "Railroad is reporting slot info with OPC_SL_RD_DATA");

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
                      objScreenManager.putError("Steal successful.");
                      NumTrainsInUse := NumTrainsInUse + 1;
                      trains(NumTrainsInUse).isConnected := true;
                      trains(NumTrainsInUse).PhyAdr := addressBeingSet;
                      trains(NumTrainsInUse).viradr := addressBeingSet;
                      trains(NumTrainsInUse).physlot := slotNumberReceived;
                      trains(NumTrainsInUse).virslot := slotNumberReceived;
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
                      objScreenManager.putError("Select successful.");
                      NumTrainsInUse := NumTrainsInUse + 1;
                      trains(NumTrainsInUse).isConnected := true;
                      trains(NumTrainsInUse).PhyAdr := addressBeingSet;
                      trains(NumTrainsInUse).viradr := addressBeingSet;
                      trains(NumTrainsInUse).physlot := slotNumberReceived;
                      trains(NumTrainsInUse).virslot := slotNumberReceived;
                      command.cmd := MoveSlots;
                      command.n := slotNumberReceived;
                      sendMessage(command);
                    end if;
                  end if;
                  putTrains(trains);
               end if;

            when OPC_LONG_ACK =>

               objScreenManager.putMessage(toString(message), "Railroad is reporting OPC_LONG_ACK");

            when OPC_MOVE_SLOTS =>

               objScreenManager.putMessage(toString(message), "Another throttle is sending OPC_MOVE_SLOTS");

            when OPC_LOCO_SPD =>

               splitLocoSpdMsg(message, slotNum, speed);
               objScreenManager.putMessage(toString(message), "Another throttle is setting speed slot " & natural'image(slotNum) & " to " & natural'image(speed));

            when OPC_LOCO_DIRF =>

               splitLocoDirfMsg(message, slotNum, trainDirection, light, horn, bell);
               objScreenManager.putMessage(toString(message), "Another throttle is setting DIRF slot " & natural'image(slotNum) & " to " & getDIRF(message));

            when OPC_LOCO_SND =>

               splitLocoSndMsg(message, slotNum, F5, F6, mute);
               objScreenManager.putMessage(toString(message), "Another throttle is setting SND slot " &natural'image(slotNum) & " to " & getSND(message));

            when OPC_SW_REQ =>

               splitSwReqMsg(message, switchNum, switchSetting);
               switchStr := toString(switchSetting);
               objScreenManager.putMessage(toString(message), "Another throttle is moving switch " & natural'image(switchNum) & " " & switchStr);

            when 16#00# =>

               -- These are the extended instructions
               case message.byteArray(2) is

                  when putTrainState =>

                     objScreenManager.putMessage(toString(message), "putTrainState " & toEnglish(message));
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

                     objScreenManager.putMessage(toString(message), "putTrainPosition " & toEnglish(message));
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

                     objScreenManager.putMessage(toString(message), "putSectionState " & toEnglish(message));

                  when putSwitchState =>

                     objScreenManager.putMessage(toString(message), "putSwitchState " & toEnglish(message));
                     splitPutSwitchStateMsg(message, switchNum, switchSetting);
                     switches(switchNum) := switchSetting;
                     objScreenManager.putError("putSwitchState processed");
                     objScreenManager.putSwitches(toString(switches));

                  when putSensorState =>

                     objScreenManager.putMessage(toString(message), "putSensorState " & toEnglish(message));

                  when putInitOutcome =>

                     -- This is assumed to be a response to a prior doLocoInit.
                     objScreenManager.putMessage(toString(message), "putInitOutcome " & toEnglish(message));

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

                     objScreenManager.putMessage(toString(message), "putReadLayoutResponse " & toEnglish(message));

                  when putTrainInformation =>

                     objScreenManager.putMessage(toString(message), "putTInfo " & toEnglish(message));
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

                     objScreenManager.putMessage(toString(message), "unknown extended message " & toEnglish(message));

                  end case;

            when others =>

               objScreenManager.putMessage(toString(message), "Unknown LocoNet message");

         end case;
		 -- ii := ii / jj;    -- checking exception handling
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadManager.Put --" & kLFString & Exception_Information (error));
            raise;
      end Put;

      procedure sendMessage(command : in out commandType) is

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
         sendThisMessage   : boolean := true;
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

               for i in trains'range loop
                 if addressBeingSet = trains(i).PhyAdr then
                   addressBeingSet := 0;
                   sendThisMessage := false;
                   objScreenManager.putError("ERROR: You have already listed this physical loco address.");
                   exit;
                 end if;
               end loop;

               if sendThisMessage and trainTableIsFull then
                   sendThisMessage := false;
                   objScreenManager.putError("ERROR: You have already set the maximum number of trains.");
               end if;

               if sendThisMessage then
                  if cmd = SelectLoco then
                     tryingToSelect := true;
                     --objScreenManager.putError("Attempting to select a physical loco address by sending OPC_LOC_ADR.");
                     objScreenManager.putError("Attempting to select a physical loco address.");
                  else
                     tryingToSteal := true;
                     --objScreenManager.putError("Attempting to steal a physical loco address by sending OPC_LOC_ADR.");
                     objScreenManager.putError("Attempting to steal a physical loco address.");
                  end if;
                  message := makeLocoAdrMsg(addressBeingSet);
               end if;

            when Forward | Backward | Horn | Bell | Light =>
            
               if not trainIdIsValid(n) then
                  sendThisMessage := false;
                  objScreenManager.putError("ERROR: A train with this id has NOT been established properly");
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
                  sendThisMessage := false;
                  objScreenManager.putError("ERROR: A train with this id has NOT been established properly");
               else
                  trains(n).Mute := toggle(trains(n).Mute);
                  message := makeLocoSndMsg(trains(n).virslot, trains(n).mute);
                  PutTrains(Trains);
               end if;

            when Speed =>

               if not trainIdIsValid(n) then
                  sendThisMessage := false;
                  objScreenManager.putError("ERROR: A train with this id has NOT been established properly");
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

               message := makeDoLocoInitMsg(Command.N, Command.Sensors);
               IF Command.N = 9999 THEN
                  -- This is a request to remove all trains
                  removeAllTrains;
                  PutTrains(Trains);
                  -- sendThisMessage := false;
                  -- objScreenManager.putError("ERROR: This feature not implemented yet");
               ELSIF getCount(command.sensors) = 0 THEN
                  -- This is a request to remove one train, which may or may not have been established
                  -- by this admin throttle.
                  -- Find out where the train is stored in the trains array and remove it.
                  -- Send the message to the controller no matter what.
                  location := getIdOfTrainFromPhyAdr(command.n);
                  IF Location = 0 THEN
                     -- The train is not in the array
                     --sendThisMessage := false;
                     objScreenManager.putError("Success: you haven't established this train's physical address but message sent to controller");
                  ELSE
                     -- The train is in the array, so get rid of it
                     -- If the train is not connected then just remove it from trains and don't send the messages else remove and send the 
                     -- message
                     if not trains(location).isConnected then
                        --sendThisMessage := false;
                        objScreenManager.putError("Success: this train was never initialized successfully but message sent to controller");
                     else
                        objScreenManager.putError("Success: message sent to the controller");
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
                        sendThisMessage := false;
                        objScreenManager.putError("ERROR: Max num trains already present");
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

               sendThisMessage := false;
               objScreenManager.putError("ERROR: Unrecongized message");

         end case;

         if sendThisMessage and cmd /= halt then
            size := sendTCPMessage(message);
            objScreenManager.putError("sending " & toEnglish(message));
         end if;

         if sendThisMessage and cmd = halt then
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