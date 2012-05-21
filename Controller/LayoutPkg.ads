WITH Ada.Unchecked_Deallocation, Ada.Strings.Unbounded, Ada.Text_IO, Globals, CommandQueueManager, Ada.Calendar;
USE Ada.Strings.Unbounded, Ada.Text_IO, Globals, CommandQueueManager, Ada.Calendar;
with MessageTranslationTypes; use messageTranslationTypes;

PACKAGE LayoutPkg IS

   ----------------------------
   ------- Sensor Types -------
   ----------------------------

   TYPE SensorObj IS PRIVATE;
   TYPE SensorObjPtr IS ACCESS SensorObj;
   TYPE SensorNode IS PRIVATE;
   TYPE SensorNodePtr IS ACCESS SensorNode;
   TYPE SensorObjList IS PRIVATE;

   ----------------------------
   ------- Switch Types -------
   ----------------------------

   TYPE SwitchObj IS PRIVATE;
   TYPE SwitchObjPtr IS ACCESS SwitchObj;
   TYPE SwitchNode IS PRIVATE;
   TYPE SwitchNodePtr IS ACCESS SwitchNode;
   TYPE SwitchObjList IS PRIVATE;

   ----------------------------
   ------ Blocking Types ------
   ----------------------------

   TYPE BlockingObj IS PRIVATE;
   TYPE BlockingObjPtr IS ACCESS BlockingObj;
   TYPE BlockingObjList IS PRIVATE;

   ----------------------------
   ------- Section Types ------
   ----------------------------

   TYPE SectionObj IS PRIVATE;
   TYPE SectionObjPtr IS ACCESS SectionObj;
   TYPE SectionNode IS PRIVATE;
   TYPE SectionNodePtr IS ACCESS SectionNode;
   TYPE SectionObjList IS PRIVATE;

   -------------------------
   ----- Train Types -------
   -------------------------

   TYPE TrainObj IS PRIVATE;
   TYPE TrainObjPtr IS ACCESS TrainObj;

   -------------------------
   ------- Exceptions ------
   -------------------------
   InvalidSwitchId   : EXCEPTION;
   InvalidSensorId   : EXCEPTION;
   CurrentSwitchNull : EXCEPTION;
   InvalidSwitchType : EXCEPTION;
   InvalidTrainId    : EXCEPTION;


   PROTECTED TYPE LayoutManager IS

      PROCEDURE IdentifyTrain (
            SensorID : Positive);
      PROCEDURE PositionTrain (
            TrainId :        TrainIdType;
            Count   :        Positive;
            Sensors :        SensorArrayType;
            Result  :    OUT Boolean);

      procedure removeFromTrainList(trainId : TrainIdType);              
      procedure freeAllSectionsOccupiedOrReservedByTrain(
            trainId :        TrainIdType);
      PROCEDURE RepositionTrain (                    -- mo 1/17/12
            TrainId :        TrainIdType;
            Count   :        Positive;
            Sensors :        SensorArrayType;
            Result  :    OUT Boolean);
      PROCEDURE AreTrainSensorsLegal (
            Count   :        positive;
            Sensors :        SensorArrayType;
            Legal   :    OUT Boolean);
      PROCEDURE MakeReservation (
            TrainId :        TrainIdType;
            Result  :    OUT Boolean);
      PROCEDURE ReleaseReservation (
            TrainId : TrainIdType);
      PROCEDURE MoveNextSwitch (
            TrainId : TrainIdType;
            State   : SwitchStateType);
      PROCEDURE MoveSwitch (
            SwitchId : Positive;
            State    : SwitchStateType);
      PROCEDURE SwitchFinishedMoving (
            SwitchId : Positive;
            State    : SwitchStateType);
      PROCEDURE GetSwitchStates;
      PROCEDURE ChangeDirectionOf (
            TrainId : TrainIdType);


      -- Creates a new section using private variable CurrentSection
      PROCEDURE NewSection (
            Id : Positive);

      -- CurrentSection has been populated with Data
      -- CurrentSection is added to the end of SectionList
      PROCEDURE EndSection;

      -- SectionList has been read
      -- Figure out the sections surrounding each section
      PROCEDURE EndSectionList;

      -- SwitchList has been read
      -- Figure out what switchs are blocking sections
      --   based on current state
      PROCEDURE EndSwitchList;

      -- Adds a new switch to CurrentSection's SwitchList
      -- If the switch is not already in the Full SwitchList, it is added
      PROCEDURE AddSwitch (
            Id    : Positive;
            State : SwitchStateType);

      -- Find the Switch with Id
      -- private variable CurrentSwitch points to the switch
      -- set the type of the switch
      PROCEDURE UpdateSwitch (
            Id           : Positive;
            TypeOfSwitch : Globals.SwitchType);

      -- Add a pointer to the sensor with Id = NarrowId to the list of narrow
      --   sensors in CurrentSwitch
      PROCEDURE UpdateSwitchNarrow (
            NarrowId : Positive);

      -- Add a pointer to the sensor with Id = ClosedId to the list of closed
      --    sensors in CurrentSwitch
      PROCEDURE UpdateSwitchClosed (
            ClosedId : Positive);

      -- Add a pointer to the sensor with Id = ThrownId to CurrentSwitch
      PROCEDURE UpdateSwitchThrown (
            ThrownId : Positive);

      -- Adds a new sensor to CurrentSection's SensorList
      -- If the sensor is not already in the Full SensorList, it is added
      PROCEDURE AddSensor (
            Id : Positive);

      -- Adds Id to CurrentSection's BlockingList
      PROCEDURE AddBlocking (
            Id : Positive);

      -- For Debugging
      PROCEDURE Print_Sections (
            Sections    : SectionObjList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean        := False);
      PROCEDURE Print_Sensors (
            Sensors     : SensorObjList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False);
      PROCEDURE Print_Switchs (
            Switchs     : SwitchObjList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False);

      --------------------------------
      ------- Get/Set Functions ------
      --------------------------------
      FUNCTION GetSectionList RETURN SectionObjList;
      FUNCTION GetSwitchList RETURN SwitchObjList;
      FUNCTION GetSensorList RETURN SensorObjList;
      FUNCTION GetXMLFilename RETURN Unbounded_String;
      PROCEDURE SetXMLFilename (Filename : Unbounded_String);

   PRIVATE
      -----------------------------
      ------- Data Members --------
      -----------------------------

      SectionList : SectionObjList;
      SensorList  : SensorObjList;
      SwitchList  : SwitchObjList;
      TrainList   : TrainObjPtr;
      XMLFilename : Unbounded_String;

      CurrentSection : SectionObjPtr;
      CurrentSwitch : SwitchObjPtr;
   END LayoutManager;

   TYPE LayoutManagerAccess IS ACCESS LayoutManager;

   -- Starts the Parsing of the XML file
   FUNCTION ParseXML (
         LayoutPtr : LayoutManagerAccess)
     RETURN Boolean;

   TASK TYPE LayoutTaskType IS
      ENTRY SetLayout (
            L : IN     LayoutManagerAccess);
   END LayoutTaskType;

PRIVATE

   -------------------------
   --------- Types ---------
   -------------------------

   TYPE LayoutObj IS TAGGED
      RECORD
         Id : Positive;
      END RECORD;

   TYPE SensorObj IS NEW LayoutObj WITH
      RECORD
         State : SensorStateType := Open;
         StartTime : Time := Clock;
      END RECORD;
      
   TYPE SensorNode IS
      RECORD
         Sensor : SensorObjPtr;
         Next   : SensorNodePtr;
      END RECORD;      
   PROCEDURE disposeBasicSensorNode IS 
      NEW Ada.Unchecked_Deallocation(Object=>SensorNode, Name=>SensorNodePtr);	   
   procedure disposeSensorNode(ptr : in out sensorNodePtr); 
         
   TYPE SensorObjList IS
      RECORD
         Head : SensorNodePtr;
         Tail : SensorNodePtr;
      END RECORD;
   procedure makeEmptySensorObjList(sol : in out sensorObjList);
      
   TYPE SwitchObj IS NEW LayoutObj WITH
      RECORD
         State         : SwitchStateType := Closed;
         TypeOfSwitch  : Globals.SwitchType  := Normal;
         ClosedSensors : SensorObjList;
         NarrowSensors : SensorObjList;
         ThrownSensor  : SensorObjPtr;
      END RECORD;
      
   TYPE SwitchNode IS
      RECORD
         Switch : SwitchObjPtr;
         Next   : SwitchNodePtr;
      END RECORD;
      
   TYPE SwitchObjList IS
      RECORD
         Head : SwitchNodePtr;
         Tail : SwitchNodePtr;
      END RECORD;

   TYPE BlockingObj IS NEW LayoutObj WITH RECORD
         Next : BlockingObjPtr;
   END RECORD;
      
   TYPE BlockingObjList IS RECORD
         Head : BlockingObjPtr;
         Tail : BlockingObjPtr;
   END RECORD;

   TYPE SectionObj IS NEW LayoutObj WITH
      RECORD
         State           : SectionStateType        := Free;
         IsUseable       : Boolean             := True;
         SensorList      : SensorObjList;
         SwitchList      : SwitchObjList;
         NextSectionList : SectionObjList;
         PrevSectionList : SectionObjList;
         BlockingList    : BlockingObjList;
         BlockCount      : Natural             := 0;
         TrainId         : TrainIdType;
      END RECORD;
      
   TYPE SectionNode IS
      RECORD
         Section : SectionObjPtr;
         Next    : SectionNodePtr;
      END RECORD;
      
   TYPE SectionObjList IS
      RECORD
         Head : SectionNodePtr;
         Tail : SectionNodePtr;
      END RECORD;

   TYPE TrainObj IS
      RECORD
         TrainId     : TrainIdType;
         SensorCount : Positive;
         SensorList  : SensorObjList;
         Queue       : CommandQueueManager.QueuePtr;
         Next        : TrainObjPtr;
      END RECORD;

   PROCEDURE disposeBasicTrainObj IS 
      NEW Ada.Unchecked_Deallocation(Object=>TrainObj, Name=>TrainObjPtr);	   
   procedure disposeTrainObj(ptr : in out TrainObjPtr); 
   
   
   PROCEDURE Free_Section IS
   NEW Ada.Unchecked_Deallocation(Object => SectionObj, Name   => SectionObjPtr);
END LayoutPkg;
