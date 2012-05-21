WITH Interfaces.C.Strings; USE Interfaces.C.Strings; USE Interfaces;
with MessageTranslationTypes; use messageTranslationTypes;
with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
WITH Ada.Unchecked_Deallocation;

PACKAGE Globals IS

	--------------------------------------------------------------------------------------
	-- Begin declarations needed by MessageTranslationLibrary

	-- TYPE DirectionType IS (Forward, Backward);
   -- TYPE OnOffType IS (Off, On);                                     
   -- TYPE SensorStateType IS (Open, Closed);
   -- TYPE SectionStateType IS (Free, Reserved, Occupied, Blocked);      
   -- TYPE TrainStateType IS (Moving, Waiting, Halted, Error, BeginChangeDirection, BeginWaiting, BeginHalted);    
   -- TYPE SwitchState IS (Closed, Thrown, BeginClosed, BeginThrown, Unknown);

   -- KMaxLenMsg   : CONSTANT Natural := 128;
   -- TYPE ByteArrayType   IS ARRAY (1 .. KMaxLenMsg) OF Unsigned_8;
   -- TYPE MessageType IS RECORD
      -- ByteArray : ByteArrayType;
      -- Size      : Integer range 0..kMaxLenMsg := 0;
   -- END RECORD;

   -- LocoNet Opcodes
   -- OPC_GPON        : CONSTANT Unsigned_8 := 16#83#; -- power on/off
   -- OPC_GPOFF       : CONSTANT Unsigned_8 := 16#82#;

   -- OPC_LOCO_SPD    : CONSTANT Unsigned_8 := 16#A0#; -- set speed
   -- OPC_LOCO_DIRF   : CONSTANT Unsigned_8 := 16#A1#; -- set direction, lights, sound
   -- OPC_LOCO_SND    : CONSTANT Unsigned_8 := 16#A2#; -- set mute, throw/close next turnout

   -- OPC_SW_REQ      : CONSTANT Unsigned_8 := 16#B0#; -- move a turnout
   -- OPC_SW_REP      : CONSTANT Unsigned_8 := 16#B1#;

   -- OPC_INPUT_REP   : CONSTANT Unsigned_8 := 16#B2#; -- sensor fired

   -- OPC_LOCO_ADR    : CONSTANT Unsigned_8 := 16#BF#; -- request slot address information
   -- OPC_SL_RD_DATA  : CONSTANT Unsigned_8 := 16#E7#; -- slot data response
   -- OPC_LONG_ACK    : CONSTANT Unsigned_8 := 16#B4#; -- insufficient slots
   -- OPC_MOVE_SLOTS  : CONSTANT Unsigned_8 := 16#BA#; -- register slot
   
   -- OPC_WR_SL_DATA  : CONSTANT Unsigned_8 := 16#EF#; -- write slot data

   -- Extended Messages
   -- PutTrainState         : CONSTANT Unsigned_8 := 16#01#; --1
   -- PutTrainPosition      : CONSTANT Unsigned_8 := 16#02#; --2
   -- PutSectionState       : CONSTANT Unsigned_8 := 16#03#; --3
   -- PutSwitchState        : CONSTANT Unsigned_8 := 16#04#; --4
   -- PutSensorState        : CONSTANT Unsigned_8 := 16#05#; --5
   -- GetSwitchSuccessor    : CONSTANT Unsigned_8 := 16#06#; --6
   -- PutSwitchSuccessor    : CONSTANT Unsigned_8 := 16#07#; --7
   -- DoLocoInit            : CONSTANT Unsigned_8 := 16#08#; --8
   -- PutInitOutcome        : CONSTANT Unsigned_8 := 16#09#; --9
   -- DoReadLayout          : CONSTANT Unsigned_8 := 16#0A#; --10
   -- PutReadLayoutResponse : CONSTANT Unsigned_8 := 16#0B#; --11

   -- MsgTryToMoveAgain     : CONSTANT Unsigned_8 := 16#10#; --16
   -- MsgFrontSensorFired   : CONSTANT Unsigned_8 := 16#11#; --17
   -- MsgBackSensorFired    : CONSTANT Unsigned_8 := 16#12#; --18
   -- MsgSensorError        : CONSTANT Unsigned_8 := 16#13#; --19
   -- MsgLoseReservation    : CONSTANT Unsigned_8 := 16#14#; --20
   -- PutTrainInformation   : CONSTANT Unsigned_8 := 16#15#; --21
   -- GetSwitchStates       : CONSTANT Unsigned_8 := 16#16#; --22
   -- getFirstSwitch        : CONSTANT Unsigned_8 := 16#17#; --23
   -- msgTrainTaskQuit      : constant unsigned_8 := 16#18#; --24
   -- msgReinitializeTrain  : constant unsigned_8 := 16#19#; --25
	
	-- End declarations needed by MessageTranslationLibrary
	--------------------------------------------------------------------------------------

   -- Simulator : Boolean := False;

   -- add in an extra slots for error handling
   -- KTrainSlots  : CONSTANT Natural := 125;
   -- KMaxSpeed    : CONSTANT Natural := 127;
   -- KNumTrains   : CONSTANT Natural := 4;
   KNumSwitches : CONSTANT Natural := 26;
   KNumSegments : CONSTANT Natural := 128;
   KMaxLenError : CONSTANT Natural := 80;
   
   kSpeedAbruptStop : constant natural := 1;      -- mo 1/9/12   0/1 = gradual/abrupt stopping
   kSpeedSlowStop   : constant natural := 1;      -- mo 1/9/12   0/1 = gradual/abrupt stopping

   WaitTime : CONSTANT duration := 1.0;                     -- mo 12/29/11
   kTrainStopDelay : constant duration := 1.5;
   -- SUBTYPE LocoAddressType IS Natural RANGE 411..4444;      -- mo 1/7/12
   kFirstVirtAddress      : constant natural := 11;     -- mo 1/15/12
   kLastVirtAddress       : constant natural := 99;     -- mo 1/15/12
   kFirstPhysAddress      : constant natural := 101;    -- mo 1/15/12
   kLastPhysAddress       : constant natural := 9999;   -- mo 1/15/12
	-- kMaxLocoAddress        : constant natural := 9999;
   kClearAllSlotsAddress  : constant natural := 9999;
   -- subtype LocoAddressType is natural range 0..kMaxLocoAddress;
   -- SUBTYPE SlotType IS Natural RANGE 0 .. KTrainSlots;
   --used for virtual and physical slots
   -- SUBTYPE TrainIdType IS natural RANGE 1..KNumTrains;
   SUBTYPE ColorType IS String(1 .. 6);
   -- SUBTYPE SpeedType IS Natural RANGE 0 .. KMaxSpeed;
   
   
   SUBTYPE SwitchID IS Natural RANGE 1 .. KNumSwitches;
   SUBTYPE SegmentID IS Natural RANGE 1 .. KNumSegments;

   --type TrainStateType is (Moving, BeginChangeDirection, Halted, Waiting, BeginHalted, BeginWaiting, Error);  -- mo


   --TYPE OnOffType IS (On, Off);                                   -- mo

   --TYPE SwitchState IS (Closed, BeginClose, Thrown, BeginThrow);  -- mo

   TYPE SwitchType IS (Normal, Crossover);

   --TYPE SectionStateType IS (Free, Occupied, Reserved, Blocked);      -- mo


   TYPE CmdType IS (PowerOn, PowerOff,              -- mo 12/14/11
      Initialize, Sel, Steal, Close, Throw, Forward, Backward,
      Increase, Decrease, Light, Horn, Bell, Mute, Halt, ReadLayout, NOP);

   KGreen   : ColorType := "green ";
   KWhite   : ColorType := "white ";
   KRed     : ColorType := "red   ";
   KYellow  : ColorType := "yellow";
   KNoColor : ColorType := "      ";

   KDeltaSpeed : CONSTANT Natural := 30;


   TYPE SwitchArrayType IS ARRAY (1 .. KNumSwitches) OF SwitchType;
   TYPE U8ArrayType IS ARRAY(Positive RANGE <>) OF Unsigned_8;
   TYPE SensorArrayType IS ARRAY(Positive RANGE <>) OF Positive;
   TYPE SensorArrayAccess IS ACCESS SensorArrayType;
   
   PROCEDURE disposeBasicSensorArray IS 
      NEW Ada.Unchecked_Deallocation(Object=>SensorArrayType, Name=>SensorArrayAccess);	   
   procedure disposeSensorArray(ptr : in out SensorArrayAccess); 
   
	procedure convertSensorArrayToList(A : in sensorArrayAccess; L : in out naturalListType);
	procedure convertSensorListToArray(L : in naturalListType; A : out sensorArrayAccess);
   
   -- TYPE TrainType IS RECORD
      -- IsConnected : Boolean := False;
      -- Color       : ColorType := KNoColor;
      -- Direction   : DirectionType := Forward;
      -- Speed       : SpeedType := 0;
      -- Slot        : SlotType := 0;
      -- VSlot       : SlotType := 0;
      -- Light       : OnOffType := Off;
      -- Horn        : OnOFfType := Off;
      -- Bell        : OnOffType := Off;
      -- Mute        : OnOFfType := Off;
      -- VirtAddress : LocoAddressType;
      -- PhysAddress : LocoAddressType;
      -- SensorCount : Natural := 0;
      -- Sensors     : SensorArrayAccess;
      -- State       : TrainStateType := Halted;
   -- END RECORD;

   -- TYPE TrainAccessType IS ACCESS TrainType;

   -- TYPE TrainArrayType  IS ARRAY (1..KNumTrains) OF TrainAccessType;

   --internal message format

   ConnectSocket   : C.Double;
   IpStrAda        : String := "127.0.0.1";
   IpStrC          : Chars_Ptr := New_String(IpStrAda);
   CEmptyString    : Chars_Ptr := New_String("");
   SimulatorPort   : C.Double := C.Double(1234);   -- for simulator 
   MessageIOPort   : C.Double := C.Double(1235);   -- for othrottles
   LocoBufferPort  : C.Double := C.Double(1236);   -- for loco buffer

   Buffer0         : C.Double := C.Double(0.0);
   BlockingMode    : C.Double := C.Double(0);
   NonblockingMode : C.Double := C.Double(1);
   CZero           : C.Double := C.Double(0);
   CValue          : C.Double;

   UZero : CONSTANT Unsigned_8 := 16#00#;

   --Constants for direction, lights, horn, bell, mute, turnout action
   KForward   : CONSTANT Unsigned_8 := UZero;
   KReverse   : CONSTANT Unsigned_8 := 16#20#; --0010 0000
   KLightsOn  : CONSTANT Unsigned_8 := 16#10#; --0001 0000
   KLightsOff : CONSTANT Unsigned_8 := Uzero;
   KHornOn    : CONSTANT Unsigned_8 := 16#02#; --0000 0010
   KHornOff   : CONSTANT Unsigned_8 := UZero;
   KBellOn    : CONSTANT Unsigned_8 := 16#01#; --0000 0001
   KBellOff   : CONSTANT Unsigned_8 := UZero;
   KMuteOn    : CONSTANT Unsigned_8 := 16#08#; --0000 1000
   KMuteOff   : CONSTANT Unsigned_8 := UZero;
   --Constants for switches
   KClosed         : CONSTANT Unsigned_8 := 16#30#; --0011 0000
   KThrown         : CONSTANT Unsigned_8 := 16#10#; --0001 0000     -- mo I use 4 constants for this stuff



END Globals;