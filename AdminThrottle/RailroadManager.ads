with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
with MessageTranslationTypes; use MessageTranslationTypes;

PACKAGE RailroadManager IS
           
   TrainsTableFull        : EXCEPTION;
   phyAdrMissingFromTable : exception;

   -- Trains
   kDeltaSpeed      : natural := 10;   
   -- kMaxLocoAddress         : Natural := 9999;
   kFirstPhysAddress       : natural := 101;
   -- SUBTYPE LocoAddressType IS Integer RANGE 0..kMaxLocoAddress;
   -- SUBTYPE SlotType        IS Integer RANGE 0..130;
   -- kMaxSpeed        : natural := 127; 
   -- SUBTYPE SpeedType       IS Integer RANGE 0..kMaxSpeed;
   TYPE    TrainType       IS RECORD
      IsConnected : Boolean := False;      -- True once the loco address in a slot has been set to in-use
      sensorsOK   : boolean := false;
      PhyAdr      : LocoAddressType := 0;  -- This is intended to be an invalid address
      PhySlot     : SlotType := 0;
      VirAdr      : LocoAddressType := 0;  -- This is intended to be an invalid address
      VirSlot     : SlotType := 0;
      State       : string(1..2) := "  ";  -- M(moving) H(halt) BH(begin halt) W(wait) BW(begin wait)
                                           -- BD(begin change direction) E(error)
      Direction   : DirectionType := Forward;
      Speed       : SpeedType := 0;
      Light       : OnOffType := Off;
      Horn        : OnOffType := Off;
      Bell        : OnOffType := Off;
      Mute        : OnOffType := Off;
      Sensors     : naturalListType;
   END RECORD;
   KNumTrains       : Integer := 4;
   TYPE TrainArrayType IS ARRAY(1..KNumTrains) OF TrainType;

   -- Switches
   KNumSwitches     : Integer := 26;
   TYPE SwitchArrayType IS ARRAY (1..KNumSwitches) OF SwitchStateType;

   -- Commands
   kMaxLenFileName         : natural := 125;  -- = KMaxLenMsg - 3 in MessageTranslationLibrary.ads
   subtype fileNameType is string(1..kMaxLenFileName);   
   type cmdType is (ReadXML, InitializeLoco, SelectLoco, StealLoco, RemoveLoco, Close, Throw, Forward, Backward,
                    Speed, Halt, Horn, Bell, Mute, Light, MoveSlots, Unknown, Quit,
                    PowerOn, PowerOff);
   type CommandType is record
      cmd        : cmdType := unknown;
      n          : natural := 0;   -- this can be one of loco address|switch#|train#
      speed      : SpeedType := 0;
      fileName   : fileNameType := (others => ' ');
      fnInUse    : natural := 0;
      sensors    : naturalListType;
   end record;

   
   
   PROTECTED TYPE RailroadManagerType IS
      procedure initialize;
      PROCEDURE Put(message : messageType);
      procedure sendMessage(command : in out commandType);
   PRIVATE  
      Trains                    : TrainArrayType;
      Switches                  : SwitchArrayType := (others => Unknown);
      LastMessageReceived       : MessageType;
      addressBeingSet           : natural := 0;
      tryingToSteal             : boolean := false;
      tryingToSelect            : boolean := false;
      numTrainsInUse            : natural := 0;    
   END RailroadManagerType;

   objRailroadManager : RailroadManagerType;
   
private
   kLFString               : string(1..1) := ( 1=> standard.ascii.LF);

end RailroadManager;