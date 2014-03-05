WITH Interfaces.C.Strings; USE Interfaces.C.Strings; USE Interfaces;
with MessageTranslationTypes; use messageTranslationTypes;
with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
WITH Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with tcpip; use tcpip;


PACKAGE ControllerGlobals IS

   kNumSwitches   : CONSTANT Natural := 26;
   kNumSegments   : CONSTANT Natural := 128;
   kMaxLenError   : CONSTANT Natural := 80;
   kMaxTrainLength : constant natural := 9; -- measured in sections
   
   kSpeedAbruptStop : constant natural := 1;      -- mo 1/9/12   0/1 = gradual/abrupt stopping
   kSpeedSlowStop   : constant natural := 1;      -- mo 1/9/12   0/1 = gradual/abrupt stopping

   WaitTime               : CONSTANT duration := 1.0;                     -- mo 12/29/11
   kTrainStopDelay 		  : constant duration := 1.5;
   kFirstVirtAddress      : constant natural := 11;     -- mo 1/15/12
   kLastVirtAddress       : constant natural := 99;     -- mo 1/15/12
   kFirstPhysAddress      : constant natural := 101;    -- mo 1/15/12
   kLastPhysAddress       : constant natural := 9999;   -- mo 1/15/12
   kClearAllSlotsAddress  : constant natural := 9999;
   SUBTYPE ColorType IS String(1 .. 6);   
   
   SUBTYPE SwitchID IS Natural RANGE 1 .. KNumSwitches;
   SUBTYPE SegmentID IS Natural RANGE 1 .. KNumSegments;

   TYPE SwitchType IS (Normal, Crossover);

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
   TYPE SensorArrayType IS ARRAY(Positive RANGE <>) OF natural;
   TYPE SensorArrayAccess IS ACCESS SensorArrayType;
   
   PROCEDURE disposeBasicSensorArray IS 
      NEW Ada.Unchecked_Deallocation(Object=>SensorArrayType, Name=>SensorArrayAccess);	   
   procedure disposeSensorArray(ptr : in out SensorArrayAccess); 
   
	procedure convertSensorArrayToList(A : in sensorArrayAccess; L : in out naturalListType);
	procedure convertSensorListToArray(L : in naturalListType; A : out sensorArrayAccess);
   
   ConnectSocket   : SocketType;
   -- ConnectSocket   : C.Double;
   IpStrAda        : unbounded_String := to_unbounded_string("0.0.0.0");
   
   Simulator       : Boolean := False;
   SimulatorPort   : unbounded_String := to_unbounded_string("1234");   -- for simulator 
   MessageIOPort   : unbounded_String := to_unbounded_string("1235");   -- for othrottles
   LocoBufferPort  : unbounded_String := to_unbounded_string("1236");   -- for loco buffer
   -- SimulatorPort   : C.Double := C.Double(1234);   -- for simulator 
   -- MessageIOPort   : C.Double := C.Double(1235);   -- for othrottles
   -- LocoBufferPort  : C.Double := C.Double(1236);   -- for loco buffer
   

   -- Buffer0         : C.Double := C.Double(0.0);
   -- BlockingMode    : C.Double := C.Double(0);
   -- NonblockingMode : C.Double := C.Double(1);
   -- CZero           : C.Double := C.Double(0);
   -- CValue          : C.Double;

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

END ControllerGlobals;