WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
USE Interfaces;
WITH Ada.Strings.Unbounded; USE Ada.Strings.Unbounded;

with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
with MessageTranslationTypes; use MessageTranslationTypes;

package MessageTranslationLibrary is
           
   -------------------------------------------------------------------------------------------
   
   function toString(switchSetting : SwitchStateType) return string;
   function toString(message : messageType) return string;
   function toEnglish(msg : messageType) return string;
  
   -------------------------------------------------------------------------------------------
   
   function getDIRF                (message : messageType) return string;
   function getSND                 (message : messageType) return string;
   
   -------------------------------------------------------------------------------------------
   
   procedure splitInputRepMsg               (message : messageType; sensor : out positive; isHi : out boolean);
   PROCEDURE splitLocoAdrMsg       			  (message : IN MessageType; Address: OUT locoAddressType);
   procedure splitLocoDirfMsg               (message : messageType; slot : out slotType; direction : out directionType;
                                             light : out onOffType; horn : out onOffType; bell : out onOffType);
   procedure splitLocoSndMsg                (message : messageType; slot : out slotType; F5, F6, mute : out onOffType);
   procedure splitLocoSpdMsg                (message : messageType; slot : out slotType; speed : out speedType);
	procedure splitLongAck                   (message : messageType; responseToOpcode : out unsigned_8; state : out SwitchStateType);
   procedure splitMoveSlots                 (message : messageType; slot : out slotType);
   procedure splitSlRdDataMsg               (message : messageType; locoAddress : out locoAddressType;
                                             isAddressAlreadyInUse : out boolean; slot : out slotType);
   procedure splitSwRepMsg                  (message : messageType; switch : out switchIdType; direction : out SwitchStateType);
   procedure splitSwReqMsg                  (message : messageType; switch : out switchIdType; direction : out SwitchStateType); 
   procedure splitSwStateMsg                (message : messageType; switch : out switchIdType);
   procedure splitWriteSlotDataToClearMsg   (msg : in messageType; slotId : out slotType);
                                      --------------------------------------
   procedure splitBackSensorFiredMsg        (msg : in MessageType; TrainId : OUT TrainIdType);
   procedure splitDoLocoInitMsg             (msg : in MessageType; locoAddress : out locoAddressType; sensors : out naturalListType);
   procedure splitDoReadLayoutMsg           (msg : in MessageType; fileName : out Unbounded_String);
   procedure splitDoRestoreStateMsg         (msg : in MessageType; x : out  integer);
   procedure splitDoSaveStateMsg            (msg : in MessageType; x : out  integer);
   procedure splitDoMakeSectionUsableMsg    (msg : in MessageType; sensor1 : out positive; sensor2 : out positive);
   PROCEDURE SplitFrontSensorFiredMsg       (msg : IN MessageType; TrainId : OUT TrainIdType);  
   procedure splitGetSwitchSuccessorMsg     (msg : in MessageType; x : out  integer);
   PROCEDURE SplitLoseReservationMsg        (msg : IN MessageType; TrainId : OUT TrainIdType);  
   procedure splitPutInitOutcomeMsg         (msg : in MessageType; physAdd : out locoAddressType; physSlot : out slotType;
                                                                   virtAdd : out locoAddressType; virtSlot : out slotType);
   procedure splitPutMakeSectionUsableResponseMsg  (msg : in MessageType; sensor1 : out positive; sensor2 : out positive; flag : out natural);
   procedure splitPutReadLayoutResponseMsg  (msg : in MessageType; responseFlag : out positive; code : out natural);
   procedure splitPutRestoreResponseMsg     (msg : in MessageType; x : out  integer);
   procedure splitPutSaveResponseMsg        (msg : in MessageType; x : out  integer);
   procedure splitPutSectionStateMsg        (msg : in MessageType; sectionId : out positive; state : out sectionStateType);
   procedure splitPutSensorStateMsg         (msg : in MessageType; sensorId : out positive; state : out sensorStateType);
   procedure splitPutSwitchStateMsg         (msg : in MessageType; switchId : out switchIdType; state : out switchStateType);
   procedure splitPutSwitchSuccessorMsg     (msg : in MessageType; x : out  integer);
   procedure splitPutTrainInformationMsg    (msg : in MessageType; 
                                             slot : out slotType; speed : out speedType; direction : out directionType;
                                             light : out onOffType; bell : out onOffType; horn : out onOffType; 
                                             mute : out onOffType); 
   procedure splitPutTrainPositionMsg       (msg : in MessageType; slot : out slotType; sensors : in out naturalListType);
   procedure splitPutTrainStateMsg          (msg : in MessageType; slot : out slotType; state : out trainStateType);
   procedure splitReinitializeTrainMsg      (msg : in messageType; trainId : out trainIdType);  
   PROCEDURE SplitSensorErrorMsg            (msg : IN MessageType; SensorNum : OUT Positive);    
   PROCEDURE SplitTrainTaskQuitMsg          (msg : IN MessageType; trainId : out trainIdType);    
                                                                                 
   -------------------------------------------------------------------------------------------
   procedure makeChecksumByte            (message : in out messageType);
   FUNCTION  makeChecksumByte            (ByteArray: ByteArrayType; Size : Integer) RETURN Unsigned_8;

   
   FUNCTION makeInputRepMsg               (Sensor : Positive; IsHigh : Boolean) RETURN MessageType;
   function makeLocoAdrMsg                (locoAddress : locoAddressType) return MessageType;
   function makeLocoDirfMsg               (slot : slotType; direction : directionType; light, horn, bell : onOffType) return MessageType;
   function makeLocoSndMsg                (slot : slotType; F5, F6, mute : onOffType := Off) return MessageType;
   function makeLocoSpdMsg                (slot : slotType; speed : speedType) return MessageType;
   function makeLongAckMsg                (opcode : unsigned_8) return MessageType; 
   function makeMoveSlotsMsg              (slot1 : slotType; slot2 : slotType) return MessageType;
   function makePowerOffMsg               return MessageType;
   function makePowerOnMsg                return MessageType;
   function makeSlRdDataMsg               (slot : slotType; address : locoAddressType) return MessageType; 
   FUNCTION makeSwRepMsg                  (Switch : switchIdType; State  : SwitchStateType) RETURN MessageType;
   function makeSwReqMsg                  (Switch : switchIdType; direction : SwitchStateType) return MessageType;
   function makeSwStateMsg                (Switch : switchIdType) return MessageType;
   function makeWriteSlotDataToClearMsg   (slotId : slotType) return MessageType;
                                      --------------------------------------   
   function makeBackSensorFiredMsg        (trainId : trainIdType) return MessageType;
   function makeDoLocoInitMsg             (locoAddress : locoAddressType; sensors : naturalListType) return messageType;
   function makeDoReadLayoutMsg           (fileName : string) return MessageType;
   function makeDoRestoreStateMsg         (x : integer) return MessageType;
   function makeDoSaveStateMsg            (x : integer) return MessageType;
   function makeDoMakeSectionUsableMsg   (sensor1 : positive; sensor2 : positive) return MessageType;
   function makeFrontSensorFiredMsg       (trainId : trainIdType) return MessageType;
   function makeGetSwitchStatesMsg        return MessageType;
   function makeGetSwitchSuccessorMsg     (x : integer) return MessageType;
   function makeLoseReservationMsg        (trainId : trainIdType) return MessageType;
   function makePutInitOutcomeMsg         (physicallocoAddress : locoAddressType; physicalSlotNum : slotType;
                                           virtuallocoAddress : locoAddressType; virtualSlotNum : slotType) return MessageType;
   function makePutMakeSectionUsableResponseMsg  (sensor1 : positive; sensor2 : positive; flag : natural) return MessageType;
	function makePutPowerChangeCompleteMsg	return MessageType;
   function makePutReadLayoutResponseMsg  (responseFlag : positive; code : natural) return MessageType;
   function makePutRestoreResponseMsg     (x : integer) return MessageType;
   function makePutSaveResponseMsg        (x : integer) return MessageType;
   function makePutSectionStateMsg        (sectionId : positive; state : sectionStateType) return MessageType;
   function makePutSensorStateMsg         (sensorId : positive; state : sensorStateType) return MessageType;
   function makePutSwitchStateMsg         (switchId : switchIdType; state : switchStateType) return MessageType;
   function makePutSwitchSuccessorMsg     (x : integer) return MessageType;
   function makePutTrainInformationMsg    (slot : slotType; speed : speedType; direction : directionType;
                                           light : onOffType; bell : onOffType; horn : onOffType; 
                                           mute : onOffType) return MessageType;
   function makePutTrainPositionMsg       (slot : slotType; sensors : naturalListType) return messageType;
   function makePutTrainStateMsg          (slot : slotType; state : trainStateType) return MessageType;
   FUNCTION makeReinitializeTrainMsg      (trainId : trainIdType) RETURN MessageType;  
   function makeSensorErrorMsg            (sensorId : positive) return MessageType;
   function makeTrainTaskQuitMsg          (trainId : trainIdType) return MessageType; 
   function makeTryToMoveAgainMsg         return MessageType;

   
   -------------------------------------------------------------------------------------------
    
private

	-- Constants for direction, lights, horn, bell, mute, turnout action
	kBackward     : constant unsigned_8 := 16#20#;  -- 0010 0000
	kLightsOn     : constant unsigned_8 := 16#10#;  -- 0001 0000 
	kHornOn       : constant unsigned_8 := 16#02#;  -- 0000 0010
	kBellOn       : constant unsigned_8 := 16#01#;  -- 0000 0001
	kMuteOn       : constant unsigned_8 := 16#08#;  -- 0000 1000 F8 on UT4
	kF5           : constant unsigned_8 := 16#01#;  -- 0000 0001 F5 on UT4
	kF6           : constant unsigned_8 := 16#02#;  -- 0000 0010 F6 on UT4

	-- Constants for switches
   kReportClosed  : constant unsigned_8 := 16#30#;
   kReportThrown  : constant unsigned_8 := 16#20#;
   kRequestClose  : constant unsigned_8 := 16#30#;
   kRequestThrow  : constant unsigned_8 := 16#10#;
   
   nullMessage    : messageType;      
   
   kLFString      : string(1..1) := ( 1=> standard.ascii.LF);  
	
end MessageTranslationLibrary;