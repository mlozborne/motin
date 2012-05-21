-- Package Specification for TrainPkg
-- Defines procedures and task to handle train information
-- Added 4/26/2011 By Alex Eveleth

WITH Globals, LayoutPkg, GenericList;
USE Globals, LayoutPkg;
with MessageTranslationTypes; use messageTranslationTypes;


PACKAGE TrainPkg IS
   PROCEDURE SpawnTrain (
      TrainId    : TrainIdType;
      TrainState : TrainStateType := Halted;
      Direction  : DirectionType  := Forward;
      Speed      : SpeedType      := 0);

   PROCEDURE SetLayout (
      L : LayoutManagerAccess);

   TASK TYPE TrainTask (TrainId : TrainIdType) IS
      ENTRY SetState     (TrainState : IN TrainStateType);
      ENTRY SetDirection (Dir : IN DirectionType);
      ENTRY SetSpeed     (Spd : IN SpeedType);
   END TrainTask;
   TYPE TrainTaskAccess IS ACCESS TrainTask;
   PACKAGE Lists IS NEW GenericList(ElementType => TrainTaskAccess);

PRIVATE
   LayoutPtr     : LayoutManagerAccess;
   TrainTaskList : Lists.List;
END TrainPkg;

