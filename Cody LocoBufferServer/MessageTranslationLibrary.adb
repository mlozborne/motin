WITH Api39dll;
USE Api39dll;

package body MessageTranslationLibrary is

   function toString (switchSetting : SwitchStateType) return string is
   begin
      if switchSetting = thrown then
         return "Thrown";
      else
         return "Closed";
      end if;
   exception
      when others =>
         return "EXCEPTION in toString of SwitchStateType in MessageTranslationLibrary";
   end;

   function toString (direction : DirectionType) return string is
   begin
      if direction = forward then
         return "Forwards";
      else
         return "Backwards";
      end if;
   exception
      when others =>
         return "EXCEPTION in toString of DirectionType in MessageTranslationLibrary";
   end;

   function toString (onOff : onOffType) return string is
   begin
      if onOFf = on then
         return "On";
      else
         return "Off";
      end if;
   exception
      when others =>
         return "EXCEPTION in toString of OnOffType in MessageTranslationLibrary";
   end;


   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   function getDIRF(message : messageType) return string is
      response : string(1..7) := "FL-B-H-";
   begin
      if (message.bytes(3) and kBackward) = kBackward then
         response(1) := 'R';
      end if;
      if (message.bytes(3) and kLightsOn) = kLightsOn then
         response(3) := '+';
      end if;
      if (message.bytes(3) and kBellOn) = kBellOn then
         response(5):= '+';
      end if;
      if (message.bytes(3) and kHornOn) = kHornOn then
         response(7) := '+';
      end if;
      return response;
   exception
      when others =>
         return "EXCEPTION in getDIRF in MessageTranslationLibrary";
   end;

   function getSND(message : messageType) return string is
      response : string(1..2) := "M-";
   begin
      if (message.bytes(3) and kMuteOn) = kMuteOn then
         response(2) := '+';
      end if;
      return response;
   exception
      when others =>
         return "EXCEPTION in getSND in MessageTranslationLibrary";
   end;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   procedure splitInputRepMsg(message : messageType; sensor : out natural) is
      a, b, c   : natural;
      bitI      : unsigned_8 := 16#20#;
   begin
      a := natural(message.bytes(2));
      b := natural(message.bytes(3) and 16#0F#);
      c := 2 * (128 * b + a + 1);
      if (message.bytes(3) and bitI) = bitI then
         -- bitI is 1
         sensor := c;
      else
         -- bitI is 0
         sensor := c - 1;
      end if;
   exception
      when others =>
         Put_Line ("EXCEPTION in splitInputRegMsg in MessageTranslationLibrary");
   end;

   procedure splitSwRepMsg(message : messageType; switch : out natural; direction : out SwitchStateType) is
   begin
      switch := 1 + natural(message.bytes(2)) + 128 * (natural(message.bytes(3) and 16#0F#));
      if (message.bytes(3) and kIsClosed) = kIsClosed then
         direction := closed;
      else
         direction := thrown;
      end if;
   exception
      when others =>
         Put_Line ("EXCEPTION in splitSwRepMsg in MessageTranslationLibrary");
   end;

   procedure splitSwReqMsg(message : messageType; switch : out natural; direction : out SwitchStateType) is
   begin
      switch := 1 + natural(message.bytes(2)) + 128 * (natural(message.bytes(3) and 16#0F#));
      if (message.bytes(3) and kIsClosed) = kIsClosed then
         direction := closed;
      else
         direction := thrown;
      end if;
   exception
      when others =>
         Put_Line ("EXCEPTION in splitSwReqMsg in MessageTranslationLibrary");
   end;

   procedure splitSlRdDataMsg(message : messageType; locoAddress : out natural;
                              isAddressAlreadyRegistered : out boolean; slot : out natural) is
      -- Build the address from adrlow (byte 5) and adrhigh (byte 10).
      -- Build the status (addressIsAlreadyRegistered) from bits 5 and 4 of byte 4.
      -- Obtain the slot # from byte 3
   begin
      locoAddress := natural(message.bytes(10)) * 128 + natural(message.bytes(5));
      isAddressAlreadyRegistered := (message.bytes(4) and 16#30#) = 16#30#;
      slot := natural(message.bytes(3));
   exception
      when others =>
         Put_Line ("EXCEPTION in splitSlRdDataMsg in MessageTranslationLibrary");
   end;

   procedure splitLocoSpdMsg(message : messageType; slot : out natural; speed : out natural) is
   begin
      slot := natural(message.bytes(2));
      speed := natural(message.bytes(3));
   exception
      when others =>
         Put_Line ("EXCEPTION in splitLocoSpdMsg in MessageTranslationLibrary");
   end;

   procedure splitLocoSndMsg(message : messageType; slot : out natural; mute : out onOffType)  is
   begin
      slot := natural(message.bytes(2));
      mute := off;
      if (message.bytes(3) and kMuteOn) = kMuteOn then
         mute := on;
      end if;
   exception
      when others =>
         Put_Line ("EXCEPTION in splitLocoSndMsg in MessageTranslationLibrary");
   end;

   procedure splitLocoDirfMsg(message : messageType; slot : out natural; direction : out directionType;
                              light : out onOffType; horn : out onOffType; bell : out onOffType) is
   begin
      slot := natural(message.bytes(2));
      direction := forward;
      if (message.bytes(3) and kBackward) = kBackward then
         direction := backward;
      end if;
      light := off;
      if (message.bytes(3) and kLightsOn) = kLightsOn then
         light := on;
      end if;
      horn := off;
      if (message.bytes(3) and kHornOn) = kHornOn then
         horn := on;
      end if;
      bell := off;
      if (message.bytes(3) and kBellOn) = kBellOn then
         bell := on;
      end if;
   exception
      when others =>
         Put_Line ("EXCEPTION in splitLocoDirfMsg in MessageTranslationLibrary");
   end;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   function sendTCPMessage(message : messageType; socket : c.double) return integer is
      BufferIdSend : C.Double := C.double (1);
      CEmptyString : Chars_Ptr := New_String ("");
      CValue : C.double;
   begin
      Cvalue := ClearBuffer(BufferIdSend);
      for i in 1..message.inuse loop
         cvalue := writeByte(c.double(message.bytes(i)), BufferIdSend);
      end loop;
      return integer(SendMessage(socket, CEmptyString, C.double (0.0), BufferIdSend));
   exception
      when others =>
         return -1;
   end;

   function receiveTCPMessageBlocking(socket : c.double) return messageType is
      size    : integer;
      message : messageType;
      BufferIdRecv : C.double := C.double (0);
      CValue : C.double;
   begin
      Cvalue := ClearBuffer (BufferIdRecv);
      Cvalue := ReceiveMessage(              -- blocking
                               Sockid => socket,
                               Len    => C.Double(0),
                               Buffid => BufferIdRecv);
      size := integer(cvalue);

      for i in 1..size loop
         message.bytes(i) := unsigned_8(readByte(BufferIdRecv));
      end loop;
      message.inuse := size;
      return message;
   exception
      when others =>
         return message;
   end;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   protected body Printing is
      procedure printMessage (message : messageType; source: SourceType) is
         switchNum, slotNum, sensorNum, locoNum, speed : natural;
         swdir : SwitchStateType;
         locodir : DirectionType;
         light, horn, bell, mute : OnOffType;
         registered, messageInLibrary : Boolean;

      begin
         messageInLibrary := False;
         --print message location
         if source = COM then
            Put ("COM: ");
         else
            Put ("TCPIP: ");
         end if;

         --print bytes
         printBytes (message);

         --Report sensor fired
         if message.bytes(1) = OPC_INPUT_REP then
            messageInLibrary := True;
            splitInputRepMsg (message, sensorNum);
            Put ("Sensor "); Put(sensorNum, width => 1); Put (" fired");
         end if;

         --Report Turnout now closed/thrown
         if message.bytes(1) = OPC_SW_REP then
            messageInLibrary := True;
            splitSwRepMsg (message, switchNum, swdir);
            Put ("Switch "); Put(switchNum, width => 1); Put (" is now "); Put (toString(swdir));
         end if;

         --Set Speed
         if message.bytes(1) = OPC_LOCO_SPD then
            messageInLibrary := True;
            splitLocoSpdMsg (message, slotNum, speed);
            Put ("Set speed of loco in slot "); Put (slotNum, width => 1); Put (" to "); Put (speed, width => 1);
         end if;

         --Set direction, horn, bell, lights
         if message.bytes(1) = OPC_LOCO_DIRF then
            messageInLibrary := True;
            splitLocoDirfMsg (message, slotNum, locodir, light, horn, bell);
            Put ("Set dirf of loco in slot "); Put (slotNum, width => 1); Put (" to the following:"); New_Line;
            Put ("Dir: "); Put (toString (locodir)); Put(" Light: "); Put(toString (light));
            Put (" Horn: "); Put (toString (horn)); Put (" Bell: "); Put (toString (bell));
         end if;

         --Set mute and unmute sound
         if message.bytes(1) = OPC_LOCO_SND then
            messageInLibrary := True;
            splitLocoSndMsg (message, slotNum, mute);
            Put ("Set mute of loco in slot "); Put (slotNum, width => 1); Put (" to "); Put (toString (mute));
         end if;

         --Move a turnout
         if message.bytes(1) = OPC_SW_REQ then
            messageInLibrary := True;
            splitSwReqMsg(message, switchNum, swdir);
            Put ("Set switch number "); Put (switchNum, width => 1); Put (" to "); Put (toString (swdir));
         end if;

         --Request for slot data
         if message.bytes(1) = OPC_LOCO_ADR then
            messageInLibrary := True;
            Put ("Request information for slot "); Put (Integer(message.bytes(3)), width => 1);
         end if;

         --Slot data response
         if message.bytes(1) = OPC_SL_RD_DATA then
            messageInLibrary := True;
            splitSlRdDataMsg (message, locoNum, registered, slotNum);
            if slotNum = 123 then
               Put ("Slot data for slot 123, fast clock");
            else
               Put ("Slot data for slot "); Put (slotNum, width => 1); Put (" containing loco address ");
               Put (locoNum, width => 1);
               if registered then
                  Put (". With loco already registered");
               else
                  Put (". With locc not registered");
               end if;
            end if;
         end if;

         --Long Ack message
         if message.bytes(1) = OPC_LONG_ACK then
            messageInLibrary := True;
            Put ("Long acknowledge message");
         end if;

         --Register Slot
         if message.bytes(1) = OPC_MOVE_SLOTS then
            messageInLibrary := True;
            Put ("Move slots with source slot of "); Put (Integer(message.bytes(2)), width => 1);
            Put (" and destination of "); Put (Integer(message.bytes(3)), width => 1);
         end if;

         --Global Power On
         if message.bytes(1) = OPC_GPON then
            messageInLibrary := True;
            Put ("Global power turned on");
         end if;

         --Global Power Off
         if message.bytes(1) = OPC_GPOFF then
            messageInLibrary := True;
            Put ("Global power turned off");
         end if;

         if messageInLibrary = false then
            Put ("Opcode not in Message Translation Library");
         end if;

         --Print new line
         New_Line;
      exception
         when others =>
            Put_Line ("EXCEPTION in printMessage of MessageTranslationLibrary");
      end; --printMessage

      procedure printBytes (message : MessageType) is
      begin
         for i in 1 .. message.InUse loop
            Put (Integer(message.bytes(i)), base => 16, width => 1);
            Put (" ");
         end loop;
         Put (": ");
      exception
         when others =>
            Put_Line ("EXCEPTION in printBytes of MessageTranslationLibrary");
      end; --printBytes
   end Printing;
   
   function getprint return print is
   begin
      return masterPrint;
   end getprint;


end MessageTranslationLibrary;
