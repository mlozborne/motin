WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
USE Interfaces;
WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;

package MessageTranslationLibrary is

   -- Constants for direction, lights, horn, bell, mute, turnout action
   kForward      : constant unsigned_8 := 16#00#;  -- 0010 0000
   kBackward     : constant unsigned_8 := 16#20#;
   kLightsOn     : constant unsigned_8 := 16#10#;  -- 0001 0000
   kLightsOff    : constant unsigned_8 := 16#00#;
   kHornOn       : constant unsigned_8 := 16#02#;  -- 0000 0010
   kHornOff      : constant unsigned_8 := 16#00#;
   kBellOn       : constant unsigned_8 := 16#01#;  -- 0000 0001
   kBellOff      : constant unsigned_8 := 16#00#;
   kMuteOn       : constant unsigned_8 := 16#08#;  -- 0000 1000
   kMuteOff      : constant unsigned_8 := 16#00#;

   -- Constants for switches
   kCloseIt       : constant unsigned_8 := 16#30#;  -- 0010 0000
   kIsClosed      : constant unsigned_8 := 16#10#;  -- 0001 0000
   kThrown        : constant unsigned_8 := 16#10#;  -- 0000 0000

   -- Opcodes
   OPC_INPUT_REP  : constant unsigned_8 := 16#B2#; -- report sensor fired
   OPC_SW_REP     : constant unsigned_8 := 16#B1#; -- report turnout now open/thrown

   OPC_LOCO_SPD   : constant unsigned_8 := 16#A0#; -- set speed
   OPC_LOCO_DIRF  : constant unsigned_8 := 16#A1#; -- set direction, horn, bell, lights
   OPC_LOCO_SND   : constant unsigned_8 := 16#A2#; -- set mute and unmute sound
   OPC_SW_REQ     : constant unsigned_8 := 16#B0#; -- move a turnout

   OPC_LOCO_ADR   : constant unsigned_8 := 16#BF#; -- request for slot data
   OPC_SL_RD_DATA : constant unsigned_8 := 16#E7#; -- slot data response
   OPC_LONG_ACK   : constant unsigned_8 := 16#B4#; -- insufficient slots
   OPC_MOVE_SLOTS : constant unsigned_8 := 16#BA#; -- register slot

   OPC_GPON : constant unsigned_8 := 16#83#;
   OPC_GPOFF : constant unsigned_8 := 16#82#;


   -- Messages
   type SourceType is (Com, TCPIP);
   type byteArrayType is array (Integer RANGE <>) of unsigned_8;
   type switchStateType is (Thrown, Closed);
   type directionType is (Forward, Backward);
   type onOffType is (On, Off);
   TYPE MessageType IS RECORD
      bytes     : byteArrayType(1..30);
      InUse     : Integer range -100000..30 := 0;
   END RECORD;

   function toString		   (switchSetting : SwitchStateType) return string;
   function toString		   (direction : DirectionType) return string;
   function toString		   (onOff : onOffType) return string;

   function getDIRF                (message : messageType) return string;
   function getSND                 (message : messageType) return string;

   procedure splitInputRepMsg      (message : messageType; sensor : out natural);
   procedure splitSwRepMsg         (message : messageType; switch : out natural; direction : out SwitchStateType);
   procedure splitSwReqMsg         (message : messageType; switch : out natural; direction : out SwitchStateType);
   procedure splitSlRdDataMsg      (message : messageType; locoAddress : out natural;
                                    isAddressAlreadyRegistered : out boolean; slot : out natural);
   procedure splitLocoSpdMsg       (message : messageType; slot : out natural; speed : out natural);
   procedure splitLocoSndMsg       (message : messageType; slot : out natural; mute : out onOffType);
   procedure splitLocoDirfMsg      (message : messageType; slot : out natural; direction : out directionType;
                                    light : out onOffType; horn : out onOffType; bell : out onOffType);

   function sendTCPMessage(message : messageType; socket : c.double) return integer;
   function receiveTCPMessageBlocking(socket : c.double) return messageType;

   protected type Printing is
      procedure printMessage	(message : messageType; source : SourceType);
      procedure printBytes		(message : messageType);
   end Printing;
   
   type Print is Access Printing;
   
   function getprint return print;
   
   masterPrint : Print := new Printing;

end MessageTranslationLibrary;
