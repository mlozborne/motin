WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
USE Interfaces;

package MessageTranslationTypes is

   -- Types of paths
   subtype pathType       is positive range 1..4;
   kDepthFirst            : constant pathType := 1;
   kBreadthFirst          : constant pathType := 2;
   kDepthFirstFreeOnly    : constant pathType := 3;
   kBreadthFirstFreeOnly  : constant pathType := 4;

   KTrainSlots             : CONSTANT Natural := 125;
   SUBTYPE SlotType        IS Natural RANGE 0 .. KTrainSlots;

   kMaxLocoAddress         : constant natural := 9999;
   subtype LocoAddressType is natural range 0..kMaxLocoAddress;

   kMaxSpeed               : natural := 127;
   SUBTYPE SpeedType       IS Integer RANGE 0..kMaxSpeed;

   KNumTrains              : CONSTANT Natural := 4;
   SUBTYPE TrainIdType     IS natural RANGE 0..KNumTrains;

   kMaxNumSwitches         : constant natural := 65000;
   subtype switchIdType    is natural range 1..kMaxNumSwitches;

   TYPE  DirectionType    IS (Forward, Backward);
   TYPE  OnOffType        IS (Off, On);
   type  sensorStateType  is (open, closed);
   type  sectionStateType is (free, reserved, occupied, blocked);
   type  trainStateType   is (moving, waiting, halted, error, beginChangeDirection, beginWaiting, beginHalted);
   type  switchStateType  is (Closed, Thrown, BeginClosed, BeginThrown, Read, Unknown);
										-- "Read" is a tempory state that indicates the controller must
										-- ask the turnout for its current state. This is used while
										-- reading the XML layout file.

   -- Messages
   KMaxLenMsg       : constant Integer := 1000; -- = 2 + 2 + maximum path length * 2
   type byteArrayType is array (1..kMaxLenMsg) of unsigned_8;
   TYPE MessageType IS RECORD
      byteArray : byteArrayType;
      Size      : Integer range 0..kMaxLenMsg := 0;
   END RECORD;

	-- Opcodes for LocoNet messages
   OPC_GPOFF      : constant unsigned_8 := 16#82#; -- power off
   OPC_GPON       : constant unsigned_8 := 16#83#; -- power on
   OPC_LOCO_SPD   : constant unsigned_8 := 16#A0#; -- set speed
   OPC_LOCO_DIRF  : constant unsigned_8 := 16#A1#; -- set direction, horn, bell, lights
   OPC_LOCO_SND   : constant unsigned_8 := 16#A2#; -- set mute and unmute sound
   OPC_SW_REQ     : constant unsigned_8 := 16#B0#; -- move a turnout
   OPC_SW_REP     : constant unsigned_8 := 16#B1#; -- report turnout now open/thrown
	OPC_SW_STATE   : constant unsigned_8 := 16#BC#; -- request state of a turnout
   OPC_INPUT_REP  : constant unsigned_8 := 16#B2#; -- report sensor fired
   OPC_LONG_ACK   : constant unsigned_8 := 16#B4#; -- if 2nd byte = 3F, then insufficient slots
	                                                -- if 2nd byte = 3C, then turnout state
   OPC_MOVE_SLOTS : constant unsigned_8 := 16#BA#; -- set a slot to in-use
   OPC_LOCO_ADR   : constant unsigned_8 := 16#BF#; -- request for slot data
   OPC_SL_RD_DATA : constant unsigned_8 := 16#E7#; -- slot data response
   OPC_WR_SL_DATA : constant unsigned_8 := 16#EF#; -- write data into a slot

   -- Opcodes for extended messages
   -- Prefix of "msg" means the message is used only inside the controller
   -- Other messages are between an OThrottle and the controller
   putTrainState                    : constant unsigned_8 := unsigned_8(1);
   putTrainPosition                 : constant unsigned_8 := unsigned_8(2);
   putSectionState                  : constant unsigned_8 := unsigned_8(3);
   putSwitchState                   : constant unsigned_8 := unsigned_8(4);
   putSensorState                   : constant unsigned_8 := unsigned_8(5);
   getSwitchSuccessor               : constant unsigned_8 := unsigned_8(6);
   putSwitchSuccessor               : constant unsigned_8 := unsigned_8(7);
   doLocoInit                       : constant unsigned_8 := unsigned_8(8);
   putInitOutcome                   : constant unsigned_8 := unsigned_8(9);
   doReadLayout                     : constant unsigned_8 := unsigned_8(10);
   putReadLayoutResponse            : constant unsigned_8 := unsigned_8(11);
   doSaveState                      : constant unsigned_8 := unsigned_8(12);
   putSaveResponse        				: constant unsigned_8 := unsigned_8(13);
   doRestoreState         				: constant unsigned_8 := unsigned_8(14);
   putRestoreResponse     				: constant unsigned_8 := unsigned_8(15);
   msgTryToMoveAgain      				: constant unsigned_8 := unsigned_8(16);
   msgFrontSensorFired    				: constant unsigned_8 := unsigned_8(17);
   msgBackSensorFired     				: constant unsigned_8 := unsigned_8(18);
   msgSensorError         				: constant unsigned_8 := unsigned_8(19);
   msgLoseReservation     				: constant unsigned_8 := unsigned_8(20);
   putTrainInformation    				: constant unsigned_8 := unsigned_8(21);
   getSwitchStates        			   : constant unsigned_8 := unsigned_8(22);
   getFirstSwitch        				: constant unsigned_8 := unsigned_8(23);
   msgTrainTaskQuit      				: constant unsigned_8 := unsigned_8(24);
   msgReinitializeTrain  				: constant unsigned_8 := unsigned_8(25);
	putPowerChangeComplete 				: constant unsigned_8 := unsigned_8(26);
   getTrainPosition                 : constant unsigned_8 := unsigned_8(31);
	doMakeSectionUsable   				: constant unsigned_8 := unsigned_8(33);
	putMakeSectionUsableResponse     : constant unsigned_8 := unsigned_8(34);
	getPath                          : constant unsigned_8 := unsigned_8(35);
	putPath                          : constant unsigned_8 := unsigned_8(36);

end MessageTranslationTypes;