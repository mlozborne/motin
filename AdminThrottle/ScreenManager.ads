WITH NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
with messageTranslationTypes;

package ScreenManager is

   PROTECTED TYPE ScreenManagerType IS
      procedure clearTheScreen;
      procedure tellUserCntrlC;
      PROCEDURE Initialize (inConMode : boolean);
      PROCEDURE PutMessage(str1 : string; str2 : String);
      -- PROCEDURE PutTrains(Trains : in out TrainArrayType);
      procedure putTrain(trainId      : messageTranslationTypes.trainIdType; 
                         phyadr       : natural; physlot : natural;
                         viradr       : natural; virslot : natural;
                         state        : string;
                         speed        : natural;
                         direction    : character;
                         light        : string;
                         bell         : string;
                         horn         : string;
                         mute         : string;
                         sensors      : naturalListType);
      -- PROCEDURE PutSwitches(Switches : SwitchArrayType);
      PROCEDURE PutSwitches(str : string);
      PROCEDURE PutError(Error : String);
      PROCEDURE PutPrompt(Prompt : String);
      -- procedure PutException(str : string);
   END ScreenManagerType;
   
   objScreenManager : screenManagerType;
   
private
   kMessageRow            : constant integer := 8;
   kSwitchesRow           : constant integer := 12;
   kErrorRow              : constant integer := 22;
   kPromptRow             : constant integer := 23;
   kExceptionRow          : constant integer := 25;            
   ExcepRowInc            : integer := 0; -- Add 4 after each use
   kLFString              : string(1..1) := ( 1=> standard.ascii.LF);
   isInControllerMode     : boolean := true;
end;