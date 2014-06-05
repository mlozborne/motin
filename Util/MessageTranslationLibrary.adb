with ada.strings.unbounded; use ada.strings.unbounded;
with ada.exceptions; use ada.exceptions;
with ada.text_io; use ada.text_io;

--with screenManager; use screenManager;

package body MessageTranslationLibrary is

   function toString(switchSetting : SwitchStateType) return string is
   begin
      if switchSetting = thrown then
         return "Thrown";
      else
         return "Closed";
      end if;
   exception
	  when error : others =>
		 put_line("****************  EXCEPTION in MessageTranslationLibrary.toString --" & kLFString & Exception_Information (error));
       raise;
   end toString;

   function toString(message : messageType) return string is
      str               : unbounded_string := null_unbounded_string;
      int1, int2, int3  : integer;
      ch1, ch2          : character;

      function decToHex(dec : natural) return character is
      begin
         case dec is
               when 0  => return '0';
               when 1  => return '1';
               when 2  => return '2';
               when 3  => return '3';
               when 4  => return '4';
               when 5  => return '5';
               when 6  => return '6';
               when 7  => return '7';
               when 8  => return '8';
               when 9  => return '9';
               when 10 => return 'A';
               when 11 => return 'B';
               when 12 => return 'C';
               when 13 => return 'D';
               when 14 => return 'E';
               when 15 => return 'F';
               when others => return '0';
         end case;
      exception
        when error : others =>
          put_line("**************** EXCEPTION in MessageTranslationLibrary.decToHex --" & kLFString & Exception_Information (error));
          raise;
      end decToHex;

   begin
	  -- Translate the message bytes into decimal and append to str
	  append(str, ":");
      for i in 1..message.size loop
         int1 := integer(message.byteArray(i));
         append(str, integer'image(int1));
      end loop;

      append(str, "/");

	  -- Translate the message bytes into hex and append to str
      for i in 1..message.size loop
         int1 := integer(message.byteArray(i));
         int2 := int1 / 16;
         int3 := int1 mod 16;
         ch1 := decToHex(int2);
         ch2 := decToHex(int3);
         append(str, ' ');
         append(str, ch1);
         append(str, ch2);
      end loop;
		append(str,":");
      return to_String(str);
   exception
      when error : others =>
         put_line("**************** EXCEPTION in MessageTranslationLibrary.toString --" & kLFString & Exception_Information (error));
         raise;
   end toString;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   function getDIRF(message : messageType) return string is
      response : string(1..7) := "FL-B-H-";
   begin
      if (message.byteArray(3) and kBackward) = kBackward then
         response(1) := 'R';
      end if;
      if (message.byteArray(3) and kLightsOn) = kLightsOn then
         response(3) := '+';
      end if;
      if (message.byteArray(3) and kBellOn) = kBellOn then
         response(5):= '+';
      end if;
      if (message.byteArray(3) and kHornOn) = kHornOn then
         response(7) := '+';
      end if;
      return response;
   exception
		when error : others =>
         put_line("**************** EXCEPTION in MessageTranslationLibrary.getDIRF --" & kLFString & Exception_Information (error));
         raise;
   end getDIRF;

   function getSND(message : messageType) return string is
      response : string(1..2) := "M-";
   begin
      if (message.byteArray(3) and kMuteOn) = kMuteOn then
         response(2) := '+';
      end if;
      return response;
    exception
	  when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.getSND --" & kLFString & Exception_Information (error));
         raise;
  end getSND;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------
   
   PROCEDURE SplitTrainTaskQuitMsg(msg : IN MessageType; trainId : out trainIdType) is 
   begin
      trainId := natural(msg.byteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.SplitTrainTaskQuitMsg --" & kLFString & Exception_Information (error));
         raise;
   end SplitTrainTaskQuitMsg;

   procedure splitReinitializeTrainMsg(msg : in messageType; trainId : out trainIdType) is
   begin
      trainId := natural(msg.byteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitReinitializeTrainMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitReinitializeTrainMsg;

   
   procedure splitWriteSlotDataToClearMsg(msg : in messageType; slotId : out slotType) is 
   begin
      slotId := natural(msg.byteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitWriteSlotDataToClearMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitWriteSlotDataToClearMsg;

   procedure splitMoveSlots(message : messageType; slot : out slotType) is
   begin
      slot := natural(message.byteArray(2));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitMoveSlots --" & kLFString & Exception_Information (error));
         raise;
   end splitMoveSlots;
      
   procedure splitInputRepMsg(message : messageType; sensor : out positive; isHi : out boolean) is
      a, b, c   : natural;
      bitI      : unsigned_8 := 16#20#;
      bitL      : unsigned_8 := 16#10#;
   begin
      a := natural(message.byteArray(2));
      b := natural(message.byteArray(3) and 16#0F#);
      c := 2 * (128 * b + a + 1);
      if (message.byteArray(3) and bitI) = bitI then
         -- bitI is 1
         sensor := c;
      else
         -- bitI is 0
         sensor := c - 1;
      end if;
      isHi := ((message.byteArray(3) and bitL) = bitL);
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitInputRepMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitInputRepMsg;

   procedure splitSwRepMsg(message : messageType; switch : out switchIdType; direction : out SwitchStateType) is
   begin
      switch := 1 + natural(message.byteArray(2)) + 128 * (natural(message.byteArray(3) and 16#0F#));
      if (message.byteArray(3) and kReportClosed) = kReportClosed then
         direction := closed;
      else
         direction := thrown;
      end if;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitSwRepMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitSwRepMsg;

   procedure splitSwReqMsg(message : messageType; switch : out switchIdType; direction : out SwitchStateType) is
   begin
      switch := 1 + natural(message.byteArray(2)) + 128 * (natural(message.byteArray(3) and 16#0F#));
      if (message.byteArray(3) and kRequestClose) = kRequestClose then
         direction := closed;
      else
         direction := thrown;
      end if;
   exception
      when error: others =>
         put_line("**************** EXCEPTION in MessageTranslationLibrary.splitSwReqMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitSwReqMsg;
	
   procedure splitSwStateMsg(message : messageType; switch : out switchIdType) is
	begin
		switch := 1 + natural(message.byteArray(2)) + 128 * (natural(message.byteArray(3) and 16#0F#));
   exception
      when error: others =>
         put_line("**************** EXCEPTION in MessageTranslationLibrary.splitSwStateMsg --" & kLFString & Exception_Information (error));
         raise;
	end splitSwStateMsg;

   procedure splitSlRdDataMsg(message : messageType; locoAddress : out locoAddressType;
                              isAddressAlreadyInUse : out boolean; slot : out slotType) is
   -- Build the address from adrlow (byte 5) and adrhigh (byte 10).
   -- Build the status (addressIsAlreadyInUse) from bits 5 and 4 of byte 4.
   -- Obtain the slot # from byte 3
   begin
      locoAddress := natural(message.byteArray(10)) * 128 + natural(message.byteArray(5));
      isAddressAlreadyInUse := (message.byteArray(4) and 16#30#) = 16#30#;
      slot := natural(message.byteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitSlRdDataMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitSlRdDataMsg;

   procedure splitLocoSpdMsg(message : messageType; slot : out slotType; speed : out speedType) is
   begin
      slot := natural(message.byteArray(2));
      speed := natural(message.byteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitLocoSpdMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitLocoSpdMsg;

   procedure splitLocoSndMsg(message : messageType; slot : out slotType; F5, F6, mute : out onOffType)  is
   begin
      slot := natural(message.byteArray(2));
		F5 := off;
		F6 := off;
      mute := off;
      if (message.byteArray(3) and kF5) = kF5 then
         F5 := on;
      end if;
      if (message.byteArray(3) and kF6) = kF6 then
         F6 := on;
      end if;
      if (message.byteArray(3) and kMuteOn) = kMuteOn then
         mute := on;
      end if;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitLocoSndMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitLocoSndMsg;

   procedure splitLocoDirfMsg(message : messageType; slot : out slotType; direction : out directionType;
                              light : out onOffType; horn : out onOffType; bell : out onOffType) is
   begin
      slot := natural(message.byteArray(2));
      direction := forward;
      if (message.byteArray(3) and kBackward) = kBackward then
         direction := backward;
      end if;
      light := off;
      if (message.byteArray(3) and kLightsOn) = kLightsOn then
         light := on;
      end if;
      horn := off;
      if (message.byteArray(3) and kHornOn) = kHornOn then
         horn := on;
      end if;
      bell := off;
      if (message.byteArray(3) and kBellOn) = kBellOn then
         bell := on;
      end if;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitLocoDirfMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitLocoDirfMsg;
   
   PROCEDURE splitLocoAdrMsg (
         message : IN MessageType;
         Address : OUT locoAddressType) IS
   BEGIN
      Address := (Integer(message.ByteArray(2)) * 128) + Integer(message.ByteArray(3));
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitLocoDirfMsg --" & kLFString & Exception_Information (error));
         RAISE;
   END SplitLocoAdrMsg;

	procedure splitLongAck(message : messageType; responseToOpcode : out unsigned_8; state : out SwitchStateType) is
	begin
		responseToOpcode := message.byteArray(2) or 16#80#;
		if responseToOpcode = OPC_SW_STATE then
			if message.byteArray(3) = 16#30# then
				state := closed;
			else	
				state := thrown;
			end if;
		end if;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitLongAck --" & kLFString & Exception_Information (error));
         RAISE;
	end splitLongAck;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   function makePowerOnMsg return MessageType is
      message : messageType;
   begin
      message.byteArray(1) := OPC_GPON;
      message.size := 2;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePowerOnMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePowerOnMsg;

   function makePowerOffMsg return MessageType is
      message : messageType;
   begin
      message.byteArray(1) := OPC_GPOFF;
	  message.size := 2;
	  makeChecksumByte(message);
	  return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePowerOffMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePowerOffMsg;

   function makeMoveSlotsMsg(slot1 : slotType; slot2 : slotType) return MessageType is
      message : messageType;
   begin
      message.byteArray(1) := OPC_MOVE_SLOTS;
      message.byteArray(2) := unsigned_8(slot1);
      message.byteArray(3) := unsigned_8(slot2);
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeMoveSlotsMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeMoveSlotsMsg;
   
   FUNCTION makeInputRepMsg (
         Sensor : Positive;
         IsHigh : Boolean)
         RETURN MessageType IS
      Cmd       :          MessageType;
      Size      : CONSTANT Positive    := 4;
      SensorNum : CONSTANT Integer     := Integer (Sensor) - 1;
   BEGIN
      Cmd.Size := Size;
      Cmd.ByteArray(1) := OPC_INPUT_REP;
      Cmd.ByteArray(2) := Unsigned_8(SensorNum mod 128) AND 16#7F#;
      Cmd.ByteArray(3) := (Unsigned_8(SensorNum / 128) AND 16#0F#) OR 16#20#;
      IF IsHigh THEN
         Cmd.ByteArray(3) := 16#10# OR Cmd.ByteArray(3);
      END IF;
      makeChecksumByte(Cmd);
      RETURN Cmd;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeInputRepMsg --" & kLFString & Exception_Information (error));
         RAISE;
   END makeInputRepMsg;

   FUNCTION makeTrainTaskQuitMsg (trainId : trainIdType) RETURN MessageType IS
      Cmd  : MessageType;
      Size : CONSTANT Positive    := 3;
   BEGIN
      Cmd.Size := Size;
      Cmd.ByteArray(1) := 16#00#;
      Cmd.ByteArray(2) := MsgTrainTaskQuit;
      Cmd.ByteArray(3) := Unsigned_8(TrainId);
      RETURN Cmd;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeTrainTaskQuitMsg --" & kLFString & Exception_Information (error));
         RAISE;
   END makeTrainTaskQuitMsg;

   FUNCTION makeReinitializeTrainMsg (trainId : trainIdType) RETURN MessageType IS    -- mo 1/28/12
      Cmd  : MessageType;
      Size : CONSTANT Positive    := 3;
   BEGIN
      Cmd.Size := Size;
      Cmd.ByteArray(1) := 16#00#;
      Cmd.ByteArray(2) := msgReinitializeTrain;
      Cmd.ByteArray(3) := Unsigned_8(TrainId);
      RETURN Cmd;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeReinitializeTrainMsg --" & kLFString & Exception_Information (error));
         RAISE;
   END makeReinitializeTrainMsg;

   FUNCTION makeSwRepMsg (
         Switch : switchIdType;
         State  : SwitchStateType)
         RETURN MessageType IS
      Cmd       :          MessageType;
      Size      : CONSTANT Positive    := 4;
      SwitchNum : CONSTANT Integer     := Integer (Switch) - 1;
   BEGIN
      Cmd.Size := Size;
      Cmd.ByteArray(1) := OPC_SW_REP;
      Cmd.ByteArray(2) := Unsigned_8(SwitchNum mod 128) AND 16#7F#;
      Cmd.ByteArray(3) := (Unsigned_8(SwitchNum / 128) AND 16#0F#) OR 16#10#;
      CASE State IS
         WHEN Closed | BeginClosed =>
            Cmd.ByteArray(3) := 16#20# OR Cmd.ByteArray(3);
         WHEN Thrown | BeginThrown | unknown | read =>
            NULL;
      END CASE;
      makeChecksumByte(Cmd);
      RETURN Cmd;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeSwRepMsg --" & kLFString & Exception_Information (error));
         RAISE;
   END makeSwRepMsg;


   function makeLocoAdrMsg(locoAddress : locoAddressType) return MessageType is
      message : messageType;
   begin
      message.byteArray(1) := OPC_LOCO_ADR;
      message.byteArray(2) := unsigned_8(locoAddress / 128);
      message.byteArray(3) := unsigned_8(locoAddress mod 128);
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeLocoAdrMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeLocoAdrMsg;

   function makeLocoDirfMsg(slot : slotType; direction : directionType; light, horn, bell : onOffType) return MessageType is
      message : messageType;
      dirf    : unsigned_8 := 16#00#;
   begin
      message.byteArray(1) := OPC_LOCO_DIRF;
      message.byteArray(2) := unsigned_8(slot);
      if direction = Backward then dirf := dirf or kBackward; end if;
      if light     = On       then dirf := dirf or kLightsOn; end if;
      if horn      = On       then dirf := dirf or kHornOn; end if;
      if bell      = On       then dirf := dirf or kBellOn; end if;
      message.byteArray(3) := dirf;
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeLocoDirfMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeLocoDirfMsg;

   function makeLocoSndMsg(slot : slotType; F5, F6, mute : onOffType := Off) return MessageType is
      message : messageType;
      snd     : unsigned_8 := 16#00#;
   begin
      message.byteArray(1) := OPC_LOCO_SND;
      message.byteArray(2) := unsigned_8(slot);
      if mute   = On then snd := snd or kMuteOn; end if;
      if F5     = On then snd := snd or kF5; end if;
      if F6     = On then snd := snd or kF6; end if;
      message.byteArray(3) := snd;
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeLocoSndMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeLocoSndMsg;

   function makeLocoSpdMsg(slot : slotType; speed : speedType) return MessageType is
      message : messageType;
   begin
      message.byteArray(1) := OPC_LOCO_SPD;
      message.byteArray(2) := unsigned_8(slot);
      message.byteArray(3) := unsigned_8(speed);
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeLocoSpdMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeLocoSpdMsg;

   function makeSwReqMsg(Switch : switchIdType; direction : SwitchStateType) return MessageType is
      message : messageType;
   begin
      message.byteArray(1) := OPC_SW_REQ;
      message.byteArray(2) := unsigned_8(switch - 1);          --        -1
      if direction = thrown then
         message.byteArray(3) := kRequestThrow;
      else
         message.byteArray(3) := kRequestClose;
      end if;
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeSwReqMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeSwReqMsg;

   function makeSwStateMsg(Switch : switchIdType) return MessageType is
		message : messageType;
	begin
      message.byteArray(1) := OPC_SW_STATE;
      message.byteArray(2) := unsigned_8(switch - 1);          --        -1
		message.byteArray(3) := 16#20#;
      message.size := 4;
      makeChecksumByte(message);
      return message;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeSwStateMsg --" & kLFString & Exception_Information (error));
         raise;
	end makeSwStateMsg;

   procedure makeChecksumByte(message : in out messageType) is
      lngth : natural := message.size;
   begin
      message.byteArray(lngth) := 16#FF#;
      for i in 1..lngth-1 loop
         message.byteArray(lngth) := message.byteArray(lngth) xor message.byteArray(i);
      end loop;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeChecksumByte procedure--" & kLFString & Exception_Information (error));
         raise;
   end makeChecksumByte;
	
   FUNCTION makeChecksumByte (
         ByteArray : ByteArrayType;
         Size      : Integer) RETURN Unsigned_8 IS
      Checksum : Unsigned_8;
   BEGIN
      Checksum := 16#ff# XOR ByteArray(1);
      FOR I IN 2..(Size-1) LOOP
         Checksum := Checksum XOR ByteArray(I);
      END LOOP;
      RETURN Checksum;
   EXCEPTION
      WHEN error: OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeChecksumByte function --" & kLFString & Exception_Information (error));
         RAISE;
   END makeChecksumByte;

------------------------------------------------------------------------------
--***********************************************************************************
--***********************************************************************************
--***********************************************************************************
--***********************************************************************************

   procedure convertNaturalToBytes(value : in natural; lowByte : out unsigned_8; highByte : out unsigned_8) is
   begin
      lowByte := unsigned_8(value mod 128);
	   highByte := unsigned_8(value / 128);
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.convertNaturalToBytes --" & kLFString & Exception_Information (error));
         raise;
   end convertNaturalToBytes;

   function convertBytesToNatural(byte1 : unsigned_8; byte2 : unsigned_8) return natural is
   begin
      return natural(byte1) + natural(byte2) * 128;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.convertBytesToNatural --" & kLFString & Exception_Information (error));
         raise;
   end convertBytesToNatural;

	function makePutPowerChangeCompleteMsg	return MessageType is
   -- <00><opcode>
		msg    : messageType := nullMessage;
	begin
		msg.byteArray(1) := 16#00#;
		msg.byteArray(2) := putPowerChangeComplete;
		msg.size := 2;
		return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutPowerChangeCompleteMsg --" & kLFString & Exception_Information (error));
         raise;
	end makePutPowerChangeCompleteMsg;
	
   function makePutTrainStateMsg (slot : slotType; state : trainStateType) return MessageType is
    -- <00><opcode><slot#><train state>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := putTrainState;
	   msg.byteArray(3) := unsigned_8(slot);
	   msg.byteArray(4) := unsigned_8(trainStateType'pos(state));
	   msg.size := 4;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutTrainStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutTrainStateMsg;

   function makePutTrainPositionMsg(slot : slotType; sensors : naturalListType) return  messageType is
   -- <00> <opcode> <slot#><count><sensor# 2bytes>...<sensor# 2 bytes>
   -- where sensor# are listed from front to back
      lowByte, highByte : unsigned_8;
	   sensorCount       : natural := getCount(sensors);
	   value             : natural;
		iter					: listIteratorType;
		msg               : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := putTrainPosition;
	   msg.byteArray(3) := unsigned_8(slot);
	   msg.byteArray(4) := unsigned_8(sensorCount);
	   iter := moveFront(sensors);
	   for i in 1..sensorCount loop
	      value := getCurrent(iter);
	   	convertNaturalToBytes(value, lowByte, highByte);
	 	   msg.byteArray(4 + 2*i-1) := lowByte;
		   msg.byteArray(4 + 2*i)   := highByte;
		   iter := moveNext(iter);
	   end loop;
	   msg.size := 4 + 2 * sensorCount;
		return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutTrainPositionMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutTrainPositionMsg;

   function makePutSectionStateMsg(sectionId : positive; state : sectionStateType) return MessageType is
   -- <00><opcode><section# 2 bytes> <section state>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := putSectionState;
	   convertNaturalToBytes(sectionId, msg.byteArray(3), msg.byteArray(4));
	   msg.byteArray(5) := unsigned_8(sectionStateType'pos(state));
	   msg.size := 5;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutSectionStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutSectionStateMsg;

   function makePutSwitchStateMsg (switchId : switchIdType; state : switchStateType) return MessageType is
   -- <00><opcode><switch#><switch state>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := putSwitchState;
	   msg.byteArray(3) := unsigned_8(switchId);
	   msg.byteArray(4) := unsigned_8(switchStateType'pos(state));
	   msg.size := 4;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutSwitchStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutSwitchStateMsg;

   function makeDoMakeSectionUsableMsg(sensor1 : positive; sensor2 : positive) return MessageType is
   -- <00><opcode><sensor1 2 bytes><sensor2 2 bytes>
		msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := doMakeSectionUsable;
	   convertNaturalToBytes(sensor1, msg.byteArray(3), msg.byteArray(4));
	   convertNaturalToBytes(sensor2, msg.byteArray(5), msg.byteArray(6));
	   msg.size := 6;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeDoMakeSectionUsableMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeDoMakeSectionUsableMsg;

   function makePutMakeSectionUsableResponseMsg(sensor1 : positive; sensor2 : positive; flag : natural) return MessageType is
   -- <00><opcode><sensor1 2 bytes><sensor2 2 bytes><flag 1 byte>
		msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := putMakeSectionUsableResponse;
	   convertNaturalToBytes(sensor1, msg.byteArray(3), msg.byteArray(4));
	   convertNaturalToBytes(sensor2, msg.byteArray(5), msg.byteArray(6));
		msg.byteArray(7) := unsigned_8(flag);
	   msg.size := 7;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutMakeSectionUsableResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutMakeSectionUsableResponseMsg;

   function makePutSensorStateMsg(sensorId : positive; state : sensorStateType) return MessageType is
   -- <00><opcode><sensor# 2 bytes><sensor state>
		msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := putSensorState;
	   convertNaturalToBytes(sensorid, msg.byteArray(3), msg.byteArray(4));
	   msg.byteArray(5) := unsigned_8(sensorStateType'pos(state));
	   msg.size := 5;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutSensorStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutSensorStateMsg;

   function makeGetSwitchSuccessorMsg     (x : integer) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeGetSwitchSuccessorMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeGetSwitchSuccessorMsg;

   function makePutSwitchSuccessorMsg     (x : integer) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutSwitchSuccessorMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutSwitchSuccessorMsg;

   function makeDoLocoInitMsg(locoAddress : locoAddressType; sensors : naturalListType) return messageType is
   -- <00><opcode><physical loco address 2 byres><count><sensor# 2 bytes>…<sensor# 2 bytes>
	  sensorCount        : natural := getCount(sensors);
	  value              : natural;
	  lowByte, highByte  : unsigned_8;
	  iter               : listIteratorType;
	  msg                : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := doLocoInit;
	   ConvertNaturalToBytes(LocoAddress, Msg.byteArray(3), Msg.byteArray(4));
      IF LocoAddress = 9999 THEN
         SensorCount := 0;
      end if;
	   msg.byteArray(5) := unsigned_8(sensorCount);
	   iter := moveFront(sensors);
	   for i in 1..sensorCount loop
	      value := getCurrent(iter);
		   convertNaturalToBytes(value, lowByte, highByte);
		   msg.byteArray(5 + 2*i-1) := lowByte;
		   msg.byteArray(5 + 2*i)   := highByte;
		   iter := moveNext(iter);
	   end loop;
	   msg.size := 5 + 2 * sensorCount;
		return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeDoLocoInitMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeDoLocoInitMsg;

   function makePutInitOutcomeMsg(physicallocoAddress : locoAddressType; physicalSlotNum : slotType;
                                  virtuallocoAddress : locoAddressType; virtualSlotNum : slotType) return MessageType is
   -- <00><opcode><physical loco address 2 bytes><slot#><virtual loco address 2 bytes><slot#>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := PutInitOutcome;
	   convertNaturalToBytes(physicalLocoAddress, msg.byteArray(3), msg.byteArray(4));
	   msg.byteArray(5) := unsigned_8(physicalSlotNum);
	   convertNaturalToBytes(virtualLocoAddress, msg.byteArray(6), msg.byteArray(7));
	   msg.byteArray(8) := unsigned_8(virtualSlotNum);
	   msg.size := 8;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutInitOutcomeMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutInitOutcomeMsg;

   function makeDoReadLayoutMsg(fileName : string) return MessageType is
   -- <00><opcode><count><XML file name>
   -- where count = number of bytes in file name
      msg : messageType := nullMessage;
	   len : natural := fileName'length;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := DoReadLayout;
	   msg.byteArray(3) := unsigned_8(len);
	   for i in 1..len loop
	       msg.byteArray(3 + i) := unsigned_8(character'pos(fileName(i)));
	   end loop;
	   msg.size := 3 + len;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeDoReadLayoutMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeDoReadLayoutMsg;

   function makePutReadLayoutResponseMsg(responseFlag : positive; code : natural) return MessageType is
   -- <00><opcode><XML read response flag><code>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := PutReadLayoutResponse;
	   msg.byteArray(3) := unsigned_8(responseFlag);
      convertNaturalToBytes(code, msg.byteArray(4), msg.byteArray(5));
	   msg.size := 5;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutReadLayoutResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutReadLayoutResponseMsg;

   function makeDoSaveStateMsg(x : integer) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeDoSaveStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeDoSaveStateMsg;

   function makePutSaveResponseMsg        (x : integer) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutSaveResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutSaveResponseMsg;

   function makeDoRestoreStateMsg         (x : integer) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeDoRestoreStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeDoRestoreStateMsg;

   function makePutRestoreResponseMsg     (x : integer) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutRestoreResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutRestoreResponseMsg;

   function makeTryToMoveAgainMsg         return MessageType is
      cmd : messageType := nullMessage;
   begin
      Cmd.Size := 2;
      Cmd.ByteArray(1) := 16#00#;
      Cmd.ByteArray(2) := MsgTryToMoveAgain;
      return cmd;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeTryToMoveAgainMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeTryToMoveAgainMsg;

   function makeFrontSensorFiredMsg       (trainId : trainIdType) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.Size := 3;
      msg.ByteArray(1) := 16#00#;
      msg.ByteArray(2) := MsgFrontSensorFired;
      msg.ByteArray(3) := Unsigned_8(TrainId);
	   return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeFrontSensorFiredMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeFrontSensorFiredMsg;

   function makeBackSensorFiredMsg        (trainId : trainIdType) return MessageType is
      msg : messageType := nullMessage;
   begin
      msg.Size := 3;
      msg.ByteArray(1) := 16#00#;
      msg.ByteArray(2) := MsgBackSensorFired;
      msg.ByteArray(3) := Unsigned_8(TrainId);
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeBackSensorFiredMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeBackSensorFiredMsg;

   function makeSensorErrorMsg            (sensorId : positive) return MessageType is
      cmd : messageType := nullMessage;
   begin
      Cmd.Size := 4;
      Cmd.ByteArray(1) := 16#00#;
      Cmd.ByteArray(2) := MsgSensorError;
      Cmd.ByteArray(3) := Unsigned_8(sensorId mod 128) AND 16#7F#;
      Cmd.ByteArray(4) := Unsigned_8(sensorId / 128) AND 16#0F#;	
      return cmd;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeSensorErrorMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeSensorErrorMsg;

   function makeLoseReservationMsg        (trainId : trainIdType) return MessageType is
      cmd : messageType := nullMessage;
   begin
      Cmd.Size := 3;
      Cmd.ByteArray(1) := 16#00#;
      Cmd.ByteArray(2) := MsgLoseReservation;
      Cmd.ByteArray(3) := Unsigned_8(TrainId);
      return cmd;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeLoseReservationMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeLoseReservationMsg;

   function makePutTrainInformationMsg    (slot : slotType; speed : speedType; direction : directionType;
                                           light : onOffType; bell : onOffType; horn : onOffType;
                                           mute : onOffType) return MessageType is
   -- <00><opcode><virtual slot#> <speed> <direction> <light> <bell> <horn> <mute>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := PutTrainInformation;
	   msg.byteArray(3) := unsigned_8(slot);
	   msg.byteArray(4) := unsigned_8(speed);
	   msg.byteArray(5) := unsigned_8(directionType'pos(direction));
	   msg.byteArray(6) := unsigned_8(onOffType'pos(light));
	   msg.byteArray(7) := unsigned_8(onOffType'pos(bell));
	   msg.byteArray(8) := unsigned_8(onOffType'pos(horn));
	   msg.byteArray(9) := unsigned_8(onOffType'pos(mute));
	   msg.size := 9;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makePutTrainInformationMsg --" & kLFString & Exception_Information (error));
         raise;
   end makePutTrainInformationMsg;

   function makeGetSwitchStatesMsg return MessageType is
   -- <00><opcode>
      msg : messageType := nullMessage;
   begin
      msg.byteArray(1) := 16#00#;
	   msg.byteArray(2) := GetSwitchStates;
	   msg.size := 2;
      return msg;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeGetSwitchStatesMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeGetSwitchStatesMsg;
   
    function makeLongAckMsg(opcode : unsigned_8) return MessageType is                           -- mo 1/12/12
      Cmd : MessageType;
   begin
      cmd.size := 4;
      cmd.byteArray(1) := opc_long_ack;
      cmd.byteArray(2) := 16#7F# and opcode;
      cmd.byteArray(3) := 16#00#;
      makeChecksumByte(cmd);
      return cmd;
   EXCEPTION
      WHEN Error : OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeLongAckMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeLongAckMsg;

   function makeSlRdDataMsg(slot : slotType; address : locoAddressType) return MessageType is   -- mo 1/15/12
      cmd                 : MessageType;
      addrLow, addrHigh   : unsigned_8;
   begin
      addrHigh := unsigned_8(address / 128);
      addrLow := unsigned_8(address rem 128);
      cmd.size := 14;
      cmd.byteArray(1)  := opc_sl_rd_data;      -- opcode
      cmd.byteArray(2)  := 16#0E#;              -- message length = 14
      cmd.byteArray(3)  := unsigned_8(slot);    -- use trainId here, will translate to virtual slot number in messageio
      cmd.byteArray(4)  := 16#3B#;              -- status = 0011 1011
      cmd.byteArray(5)  := addrLow;             -- address low
      cmd.byteArray(6)  := 16#00#;              -- speed = 0
      cmd.byteArray(7)  := 16#00#;              -- dirf
      cmd.byteArray(8)  := 16#00#;              -- trk
      cmd.byteArray(9)  := 16#00#;              -- ss2
      cmd.byteArray(10) := addrHigh;            -- address high
      cmd.byteArray(11) := 16#00#;              -- snd
      cmd.byteArray(12) := 16#00#;              -- id1
      cmd.byteArray(13) := 16#00#;              -- id2
      makeChecksumByte(cmd);
      return cmd;
   EXCEPTION
      WHEN Error : OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeSlRdDataMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeSlRdDataMsg;
   
   function makeWriteSlotDataToClearMsg(slotId : slotType) return MessageType is   -- mo 1/15/12
      cmd                 : MessageType;
      addrLow, addrHigh   : unsigned_8;
   begin
      addrHigh := 0;
      addrLow := 0;
      cmd.size := 14;
      cmd.byteArray(1)  := OPC_WR_SL_DATA;      -- opcode
      cmd.byteArray(2)  := 16#0E#;              -- message length = 14
      cmd.byteArray(3)  := unsigned_8(slotId);   -- use trainId here, will translate to virtual slot number in messageio
      cmd.byteArray(4)  := 16#0B#;              -- status = 0000 1011
      cmd.byteArray(5)  := addrLow;             -- address low
      cmd.byteArray(6)  := 16#00#;              -- speed = 0
      cmd.byteArray(7)  := 16#00#;              -- dirf
      cmd.byteArray(8)  := 16#00#;              -- trk
      cmd.byteArray(9)  := 16#00#;              -- ss2
      cmd.byteArray(10) := addrHigh;            -- address high
      cmd.byteArray(11) := 16#00#;              -- snd
      cmd.byteArray(12) := 16#00#;              -- id1
      cmd.byteArray(13) := 16#00#;              -- id2
      makeChecksumByte(cmd);
      return cmd;
   EXCEPTION
      WHEN Error : OTHERS =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.makeWriteSlotDataToClearMsg --" & kLFString & Exception_Information (error));
         raise;
   end makeWriteSlotDataToClearMsg;
   
   -------------------------------------------------------------------------------------------
   -------------------------------------------------------------------------------------------
   -------------------------------------------------------------------------------------------

   procedure splitPutTrainStateMsg(msg : in MessageType; slot : out slotType; state : out trainStateType) is
    -- <00><opcode><slot#><train state>
   begin
      slot := natural(msg.byteArray(3));
      state := trainStateType'val(natural(msg.byteArray(4)));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutTrainStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutTrainStateMsg;

   procedure splitPutTrainPositionMsg(msg : in MessageType; slot : out slotType; sensors : in out naturalListType) is
   -- <00> <opcode> <slot#><count><sensor# 2bytes>...<sensor# 2 bytes>
   -- where sensor# are listed from front to back
      sensorCount : natural;
   begin
      slot := natural(msg.byteArray(3));
	   sensorCount := natural(msg.byteArray(4));
	   makeEmpty(sensors);
	   for i in 1..sensorCount loop
	      addEnd(sensors, convertBytesToNatural(msg.byteArray(4 + 2*i-1), msg.byteArray(4 + 2*i)));
      end loop;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutTrainPositionMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutTrainPositionMsg;

   procedure splitPutSectionStateMsg(msg : in MessageType; sectionId : out positive; state : out sectionStateType) is
   -- <00><opcode><section# 2 bytes> <section state>
   begin
      sectionId := convertBytesToNatural(msg.byteArray(3), msg.byteArray(4));
      state := sectionStateType'val(natural(msg.byteArray(5)));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutSectionStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutSectionStateMsg;

   procedure splitPutSwitchStateMsg(msg : in MessageType; switchId : out switchIdType; state : out switchStateType) is
   -- <00><opcode><switch#><switch state>
   begin
      switchId := positive(msg.byteArray(3));
      state := switchStateType'val(natural(msg.byteArray(4)));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutSwitchStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutSwitchStateMsg;

   procedure splitDoMakeSectionUsableMsg   (msg : in MessageType; sensor1 : out positive; sensor2 : out positive) is
   -- <00><opcode><sensor1 2 bytes><sensor2 2 bytes>
   begin
      sensor1 := convertBytesToNatural(msg.byteArray(3), msg.byteArray(4));
      sensor2 := convertBytesToNatural(msg.byteArray(5), msg.byteArray(6));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitDoMakeSectionUsableMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitDoMakeSectionUsableMsg;

   procedure splitPutMakeSectionUsableResponseMsg  (msg : in MessageType; sensor1 : out positive; sensor2 : out positive; flag : out natural) is
   -- <00><opcode><sensor# 2 bytes><sensor state>
   begin
      sensor1 := convertBytesToNatural(msg.byteArray(3), msg.byteArray(4));
      sensor2 := convertBytesToNatural(msg.byteArray(5), msg.byteArray(6));
      flag := natural(msg.byteArray(7));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutMakeSectionUsableResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutMakeSectionUsableResponseMsg;

   procedure splitPutSensorStateMsg (msg : in MessageType; sensorId : out positive; state : out sensorStateType) is
   -- <00><opcode><sensor# 2 bytes><sensor state>
   begin
      sensorId := convertBytesToNatural(msg.byteArray(3), msg.byteArray(4));
      state := sensorStateType'val(natural(msg.byteArray(5)));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutSensorStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutSensorStateMsg;

   procedure splitGetSwitchSuccessorMsg     (msg : in MessageType; x : out  integer) is
   begin
      x := 0;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitGetSwitchSuccessorMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitGetSwitchSuccessorMsg;

   procedure splitPutSwitchSuccessorMsg     (msg : in MessageType; x : out  integer) is
   begin
      x := 0;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutSwitchSuccessorMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutSwitchSuccessorMsg;

   procedure splitDoLocoInitMsg(msg : in MessageType; locoAddress : out locoAddressType; sensors : out naturalListType) is
   -- <00><opcode><physical loco address 2 byres><count><sensor# 2 bytes>…<sensor# 2 bytes>
      sensorCount : natural;
      L         : naturalListType;
      --x M         : naturalListType;
   begin
      makeEmpty(L);
      locoAddress := convertBytesToNatural(msg.byteArray(3), msg.byteArray(4));
	   sensorCount := natural(msg.byteArray(5));
	   for i in 1..sensorCount loop		
	      addEnd(L, convertBytesToNatural(msg.byteArray(5 + 2*i-1), msg.byteArray(5 + 2*i)));
      end loop;
      sensors := L;
      --x addFront(M,1); makeEmpty(M);
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in MessageTranslationLibrary.splitDoLocoInitMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitDoLocoInitMsg;

   procedure splitPutInitOutcomeMsg(msg : in MessageType; physAdd : out locoAddressType; physslot : out slotType;
                                                          virtAdd : out locoAddressType; virtslot : out slotType) is
   -- <00><opcode><physical loco address 2 bytes><slot#><virtual loco address 2 bytes><slot#>
   begin
      physAdd := convertBytesToNatural(msg.byteArray(3), msg.byteArray(4));
      physSlot := natural(msg.byteArray(5));
      virtAdd := convertBytesToNatural(msg.byteArray(6), msg.byteArray(7));
      virtSlot := natural(msg.byteArray(8));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutInitOutcomeMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutInitOutcomeMsg;

   procedure splitDoReadLayoutMsg(msg : in MessageType; fileName : out Unbounded_String) is
   -- <00><opcode><count><XML file name>
		count : natural;
   begin
      Count := natural(msg.ByteArray(3));
      Filename := Null_Unbounded_String;
      FOR I IN 4..Count+3 LOOP
         Append(Filename, Character'Val(Integer(msg.ByteArray(I))));
      END LOOP;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitDoReadLayoutMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitDoReadLayoutMsg;

   procedure splitPutReadLayoutResponseMsg(msg : in MessageType; responseFlag : out positive; code : out natural) is
   -- <00><opcode><XML read response flag><code>
   begin
      responseFlag := natural(msg.byteArray(3));
      code := convertBytesToNatural(msg.byteArray(4), msg.byteArray(5));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutReadLayoutResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutReadLayoutResponseMsg;

   procedure splitDoSaveStateMsg            (msg : in MessageType; x : out  integer) is
   begin
      x := 0;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitDoSaveStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitDoSaveStateMsg;

   procedure splitPutSaveResponseMsg        (msg : in MessageType; x : out  integer) is
   begin
      x := 0;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutSaveResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutSaveResponseMsg;

   procedure splitDoRestoreStateMsg         (msg : in MessageType; x : out  integer) is
   begin
      x := 0;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitDoRestoreStateMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitDoRestoreStateMsg;

   procedure splitPutRestoreResponseMsg     (msg : in MessageType; x : out  integer) is
   begin
      x := 0;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutRestoreResponseMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutRestoreResponseMsg;

   -- procedure splitTryToMoveAgainMsg         (msg : in MessageType; x : out  integer) is
   -- begin
      -- x := 0;
   -- exception
	   -- when error : others =>
		   -- put_line("**************** EXCEPTION in MessageTranslationLibrary.splitTryToMoveAgainMsg --" & kLFString & Exception_Information (error));
         -- raise;
   -- end splitTryToMoveAgainMsg;

   PROCEDURE SplitFrontSensorFiredMsg       (msg : IN MessageType; TrainId : OUT TrainIdType) is  
   begin
      TrainId := trainIdType(msg.ByteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitFrontSensorFiredMsg --" & kLFString & Exception_Information (error));
         raise;   
   end splitFrontSensorFiredMsg;

   procedure splitBackSensorFiredMsg        (msg : in MessageType; TrainId : OUT TrainIdType) is
   begin
      TrainId := trainIdType(msg.ByteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitBackSensorFiredMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitBackSensorFiredMsg;

   PROCEDURE SplitSensorErrorMsg            (msg : IN MessageType; SensorNum : OUT Positive) is    
   begin
      IF msg.ByteArray(3) /= 16#00# THEN
         SensorNum := Positive(msg.ByteArray(3));
         SensorNum := SensorNum + (128 * Integer(msg.ByteArray(4) AND 16#0F#));
      ELSE
         SensorNum := (128 * Integer(msg.ByteArray(4) AND 16#0F#));
      END IF;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitSensorErrorMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitSensorErrorMsg;

   PROCEDURE SplitLoseReservationMsg        (msg : IN MessageType; TrainId : OUT TrainIdType) is 
   begin
      TrainId := trainIdType(msg.ByteArray(3));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitLoseReservationMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitLoseReservationMsg;

   procedure splitPutTrainInformationMsg(msg : in MessageType;
                                         slot : out slotType; speed : out speedType; direction : out directionType;
                                         light : out onOffType; bell : out onOffType; horn : out onOffType;
                                         mute : out onOffType) is
   -- <00><opcode><virtual slot#> <speed> <direction> <light> <bell> <horn> <mute>
   begin
      slot := natural(msg.byteArray(3));
      speed := natural(msg.byteArray(4));
      direction := directionType'val(natural(msg.byteArray(5)));
      light := onOffType'val(natural(msg.byteArray(6)));
      bell := onOffType'val(natural(msg.byteArray(7)));
      horn := onOffType'val(natural(msg.byteArray(8)));
      mute := onOffType'val(natural(msg.byteArray(9)));
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.splitPutTrainInformationMsg --" & kLFString & Exception_Information (error));
         raise;
   end splitPutTrainInformationMsg;

   --***********************************************************************************

   function toEnglish(msg : messageType) return string is
      slotNum, sectionId, switchId, physAdd, locoAdd,
      physSlot, virtAdd, virtSlot, responseFlag, trainId,
      responseCode, speed, sensorId     : natural;
		sensor1, sensor2						 : positive;
		flag										 : natural;
      trainState                        : trainStateType;
      sectionState                      : sectionStateType;
      switchState                       : switchStateType;
      sensorState                       : sensorStateType;
      sensors                           : naturalListType;
		count                             : natural;
      direction                         : directionType;
      light, bell, horn, mute, F5, F6   : onOffType;
      isHi                              : boolean;
      inUse                             : boolean;
		responseTo								 : unsigned_8;
   begin
      if msg.byteArray(1) /= 16#00# then
         case msg.byteArray(1) is
            when OPC_GPON =>
               return "OPC_GPON power on ";
            when OPC_GPOFF =>
               return "OPC_GPOFF power off ";
            when OPC_INPUT_REP =>
               splitInputRepMsg(msg, sensorId, isHi);
					if isHi then
						return "OPC_INPUT_REP sensor hi" & natural'image(sensorId);
					else
						return "OPC_INPUT_REP sensor lo" & natural'image(sensorId);
					end if;
            when OPC_SW_REP =>
               splitSwRepMsg(msg, switchId, switchState);
               return "OPC_SW_REP switch" & natural'image(switchId) & " " & switchStateType'image(switchState);
            when OPC_SW_REQ =>
               splitSwReqMsg(msg, switchId, switchState); 
               return "OPC_SW_REQ switch" & natural'image(switchId) & " " & switchStateType'image(switchState);
				when OPC_SW_STATE =>
					splitSwStateMsg(msg, switchId);
					return "OPC_SW_STATE switch" & natural'image(switchId);
				when OPC_LOCO_SPD =>
               splitLocoSpdMsg(msg, trainId, speed);
               return "OPC_LOCO_SPD [(trainId or slot)/speed] [" & natural'image(trainId) & "/" & natural'image(speed) & "]";
            when OPC_LOCO_DIRF =>
                splitLocoDirfMsg(msg, trainId, direction, light, horn, bell);
                return "OPC_LOCO_DIRF"
                       & " (trainId or slot)" & natural'image(trainId) 
                       & " dir "  & directionType'image(direction)
                       & " l " & onOffType'image(light) 
                       & " b " & onOffType'image(bell) 
                       & " h " & onOffType'image(horn);

            when OPC_LOCO_SND =>
                splitLocoSndMsg(msg, trainId, F5, F6, mute);
                return "OPC_LOCO_SND"
                       & " (trainId or slot)" & natural'image(trainId) 
                       & " F5 " & onOffType'image(F5) 
                       & " F6 " & onOffType'image(F6) 
                       & " mute " & onOffType'image(mute);                                             
            when OPC_LOCO_ADR =>
               splitLocoAdrMsg(msg, locoAdd);
               return "OPC_LOCO_ADR request slot data for loco " & natural'image(locoAdd);
            when OPC_SL_RD_DATA =>
               splitSlRdDataMsg(msg, locoAdd, inUse, slotNum);
               return "OPC_SL_RD_DATA loco/inUse/slotNum " & natural'image(locoAdd) & " " & boolean'image(inUse) & " " & natural'image(slotNum);
            when OPC_LONG_ACK =>
				   splitLongAck(msg, responseTo, switchState);
					case responseTo is
						when OPC_GPOFF =>
							return "OPC_LONG_ACK to OPC_GPOFF";
						when OPC_GPON =>
							return "OPC_LONG_ACK to OPC_GPON";
						when OPC_LOCO_SPD =>
							return "OPC_LONG_ACK to OPC_LOCO_SPD";
						when OPC_LOCO_DIRF =>
							return "OPC_LONG_ACK to OPC_LOCO_DIRF";
						when OPC_LOCO_SND =>
							return "OPC_LONG_ACK to OPC_LOCO_SND";
						when OPC_SW_REQ =>
							return "OPC_LONG_ACK to OPC_SW_REQ";
						when OPC_SW_REP =>
							return "OPC_LONG_ACK to OPC_SW_REP";
						when OPC_SW_STATE =>
							return "OPC_LONG_ACK to OPC_SW_STATE: state = " & switchStateType'image(switchState);
						when OPC_INPUT_REP =>
							return "OPC_LONG_ACK to OPC_INPUT_REP";
						when OPC_LONG_ACK =>
							return "OPC_LONG_ACK to OPC_LONG_ACK";
						when OPC_MOVE_SLOTS =>
							return "OPC_LONG_ACK to OPC_MOVE_SLOTS";
						when OPC_LOCO_ADR =>
							return "OPC_LONG_ACK to OPC_LOCO_ADR: insufficient slots";
						when OPC_SL_RD_DATA =>
							return "OPC_LONG_ACK to OPC_SL_RD_DATA";
						when OPC_WR_SL_DATA =>
							return "OPC_LONG_ACK to OPC_WR_SL_DATA";
						when others =>
							return "OPC_LONG_ACK to unexpected op " & unsigned_8'image(responseTo);
					end case;
            when OPC_MOVE_SLOTS =>
               splitMoveSlots(msg, slotNum);
               return "OPC_MOVE_SLOTS for slot " & natural'image(slotNum);
            when OPC_WR_SL_DATA =>
               splitWriteSlotDataToClearMsg(msg, slotNum);
               return "OPC_WR_SL_DATA write data into slot " & natural'image(slotNum);
            when others =>
               return "Unknown LocoNet opcode: " & toString(msg);
         end case;
      end if; 

      case msg.byteArray(2) is
			when putPowerChangeComplete =>
				return "putPowerChangeComplete";
         when putTrainState =>
            splitPutTrainStateMsg(msg, slotNum, trainState);
            return "putTrainState: [(trainId or slot)/state] [" & natural'image(slotNum) & "/" & trainStateType'image(trainState) & "]";
         when putTrainPosition =>
            splitPutTrainPositionMsg(msg, slotNum, sensors);
				count := getCount(sensors);
				makeEmpty(sensors);
            return "putTrainPosition: [(trainId or slot)/num sensors] [" & natural'image(slotNum) & "/" & natural'image(count) & "]";
         when putPath =>
            xxxx
         when putSectionState =>
            splitPutSectionStateMsg(msg, sectionId, sectionState);
            return "putSectionState: sectionId/state" & natural'image(sectionId) & " " & sectionStateType'image(sectionState);
         when putSwitchState =>
            splitPutSwitchStateMsg(msg, switchId, switchState);
            return "putSwitchState: switch/position" & natural'image(switchId) & " " & switchStateType'image(switchState);
         when putSensorState =>
            splitPutSensorStateMsg(msg, sensorId, sensorState);
            return "putSensorState: sensor/state" & natural'image(sensorId) & " " & sensorStateType'image(sensorState);
         when getSwitchSuccessor =>
            return "getSwitchSuccessor";
         when putSwitchSuccessor =>
            return "putSwitchSuccessor";
         when doLocoInit =>
            splitDoLocoInitMsg(msg, locoAdd, sensors);
            return "doLocoInit: loco/numSensors" & natural'image(locoAdd) & " " & natural'image(getCount(sensors));
         when putInitOutcome =>
            splitPutInitOutcomeMsg(msg, physAdd, physSlot, virtAdd, virtSlot);
            return "putInitOutcome: padd" & natural'image(physAdd) & " pslot" & natural'image(physSlot) &
                   " vadd" & natural'image(virtAdd) & " vslot" & natural'image(virtSlot);
         when doReadLayout =>
            return "doReadLayout";
         when putReadLayoutResponse =>
            splitPutReadLayoutResponseMsg(msg, responseFlag, responseCode);
            return "putReadLayoutResponse: flag/code" & natural'image(responseFlag) & " " & natural'image(responseCode);
         when doSaveState =>
            return "doSaveState";
         when putSaveResponse =>
            return "putSaveResponse";
         when doRestoreState =>
            return "doRestoreState";
         when putRestoreResponse =>
            return "putRestoreResponse";
         when msgTryToMoveAgain =>
            return "msgTryToMoveAgain";
         when msgFrontSensorFired =>
            SplitFrontSensorFiredMsg(msg, TrainId);  
            return "msgFrontSensorFired for train " & natural'image(trainId);
         when msgBackSensorFired =>
            SplitBackSensorFiredMsg(msg, TrainId);  
            return "msgBackSensorFired for train " & natural'image(trainId);
         when msgSensorError =>
            SplitSensorErrorMsg(msg, SensorId);    
            return "msgSensorError for sensor " & natural'image(sensorId);
         when msgLoseReservation =>
            SplitLoseReservationMsg(msg, TrainId);  
            return "msgLoseReservation for train " & natural'image(trainId);
         when putTrainInformation =>
            splitPutTrainInformationMsg(msg, slotNum, speed, direction, light, bell, horn, mute);
            return "putTrainInfo: [(trainId or slot) sp dir l b h m] [" &
                   natural'image(slotNum) & natural'image(speed) & " " & directionType'image(direction) & 
                   " " & onOffType'image(light) & " " & onOffType'image(bell) & 
						 " " & onOffType'image(horn) & " " &onOffType'image(mute) & "]";
			when doMakeSectionUsable =>
				splitDoMakeSectionUsableMsg(msg, sensor1, sensor2);
				return "doMakeSectionUsable: sensor1/sensor2 " & 
				       natural'image(sensor1) & "/" &
				       natural'image(sensor2);
			when putMakeSectionUsableResponse =>
				splitPutMakeSectionUsableResponseMsg(msg, sensor1, sensor2, flag);
				return "doMakeSectionUsable: sensor1/sensor2/flag " & 
				       natural'image(sensor1) & "/" &
				       natural'image(sensor2) & "/" &
				       natural'image(flag);
         when getSwitchStates =>
            return "getSwitchStates";
         when getFirstSwitch =>
            return "getFirstSwitch";
         when msgTrainTaskQuit =>
            SplitTrainTaskQuitMsg(msg, trainId);
            return "msgTrainTaskQuit for train " & natural'image(trainId);
         when msgReinitializeTrain =>
            splitReinitializeTrainMsg(msg, trainId);
            return "msgReinitializeTrain for train " & natural'image(trainId);
         when others =>
            return "Unknown extended opcode: " & toString(msg);
      end case;
   exception
	   when error : others =>
		   put_line("**************** EXCEPTION in MessageTranslationLibrary.toEnglish --" & kLFString & Exception_Information (error));
         raise;
   end toEnglish;

end MessageTranslationLibrary;