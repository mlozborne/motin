WITH Ada.Unchecked_Deallocation, Ada.Strings.Unbounded, Ada.Text_IO, ControllerGlobals, CommandQueueManager, Ada.Calendar;
USE Ada.Strings.Unbounded, Ada.Text_IO, ControllerGlobals, CommandQueueManager, Ada.Calendar;
with MessageTranslationTypes; use messageTranslationTypes;
with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;

PACKAGE LayoutPkg IS

-- Contents
--   1. Declarations of types used by LayoutManagerPkg
--   2. LayoutManager and LayoutManager object
--      a. Functionss to manipulate data structures
--      b. Build data structures from XML
--      c. Debug print data structures
--      d. Get/Set Functions
--      e. Data structures
--      f. Helper functions
--   3. LayoutTaskType
--   4. Definition of types used by LayoutManagerPkg

	------------------------ 1 ----------------------------------
	------- Begin declarations of types used by LayoutManagerPkg 
   TYPE SensorObj IS PRIVATE;
   TYPE SensorObjPtr IS ACCESS SensorObj;
   TYPE SensorNode IS PRIVATE;
   TYPE SensorNodePtr IS ACCESS SensorNode;
   TYPE SensorObjList IS PRIVATE;

   TYPE SwitchObj IS PRIVATE;
   TYPE SwitchObjPtr IS ACCESS SwitchObj;
   TYPE SwitchNode IS PRIVATE;
   TYPE SwitchNodePtr IS ACCESS SwitchNode;
   TYPE SwitchObjList IS PRIVATE;

   TYPE BlockingObj IS PRIVATE;
   TYPE BlockingObjPtr IS ACCESS BlockingObj;
   TYPE BlockingObjList IS PRIVATE;

   TYPE SectionObj IS PRIVATE;
   TYPE SectionObjPtr IS ACCESS SectionObj;
   TYPE SectionNode IS PRIVATE;
   TYPE SectionNodePtr IS ACCESS SectionNode;
   TYPE SectionObjList IS PRIVATE;

   TYPE TrainObj IS PRIVATE;
   TYPE TrainObjPtr IS ACCESS TrainObj;
	
	PROCEDURE SendToOutQueue(Cmd : MessageType);

   ------- Exceptions ------
   InvalidSwitchId   : EXCEPTION;
   InvalidSensorId   : EXCEPTION;
   CurrentSwitchNull : EXCEPTION;
   InvalidSwitchType : EXCEPTION;
   InvalidTrainId    : EXCEPTION;
   ---- End declarations of types used by LayoutManager 
   ------------------------- 1 --------------------------------


	-------------------------- 2 -------------------------------------
	-------------------- Begin LayoutManager -------------------------
   PROTECTED TYPE LayoutManager IS

      ----------------------- 2a ----------------------------
      -- Begin functions to manipulate data structures 	
      PROCEDURE AreTrainSensorsLegal (Count   :        positive;
												  Sensors :        SensorArrayType;
												  Legal   :    OUT Boolean);
      PROCEDURE ChangeDirectionOf    (TrainId : TrainIdType);
      procedure freeAllSectionsOccupiedOrReservedByTrain(trainId : TrainIdType);
      PROCEDURE GetSwitchStates;
		PROCEDURE IdentifyTrainV1      (SensorID : Positive);  -- section oriented, original, assumes no sensor errors
		PROCEDURE IdentifyTrainV2      (SensorID : Positive);  -- sensor oriented, assumes no sensor errors
		PROCEDURE IdentifyTrainV3      (SensorID : Positive);  -- sensor oriented, assumes sensor errors
      PROCEDURE MakeReservation      (TrainId :        TrainIdType;
                                      Result  :    OUT Boolean);
      PROCEDURE MoveNextSwitch       (TrainId : TrainIdType;
                                      State   : SwitchStateType);
      PROCEDURE MoveSwitch           (SwitchId : Positive;
                                      State    : SwitchStateType);
      PROCEDURE PositionTrain       (TrainId :        TrainIdType;
                                     Count   :        Positive;
                                      Sensors :        SensorArrayType;
                                      Result  :    OUT Boolean);
      PROCEDURE ReleaseReservation   (TrainId : TrainIdType);
      procedure removeFromTrainList  (trainId : TrainIdType);              
      PROCEDURE RepositionTrain      (TrainId :        TrainIdType;
                                      Count   :        Positive;
                                      Sensors :        SensorArrayType;
                                      Result  :    OUT Boolean);
		procedure setAllSensorsOpen;		
      PROCEDURE SwitchFinishedMoving (SwitchId : Positive;
                                      State    : SwitchStateType);
      -- End functions to manipulate data structures 	
      ------------------------ 2a -------------------------------

		------------------------------- 2b -----------------------------------------
		-------------------- Begin build data structures from XML ------------------
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
            Id    : Positive);

      -- Find the Switch with Id
      -- private variable CurrentSwitch points to the switch
      -- set the type of the switch
      PROCEDURE UpdateSwitch (
            Id           : Positive;
            TypeOfSwitch : ControllerGlobals.SwitchType;
            state        : switchStateType);

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
		-------------------- End build data structures from XML --------------------
		------------------------------ 2b ------------------------------------------


		------------------------------- 2c ----------------------------------
		----------------- Begin debug print data structures -----------------
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
		----------------- End debug print data structures ------------------
		---------------------------- 2c ------------------------------------


      -------------- 2d --------------------
      ------- Begin Get/Set Functions ------      
      FUNCTION GetXMLFilename RETURN Unbounded_String;
      PROCEDURE SetXMLFilename (Filename : Unbounded_String);
      ------- End Get/Set Functions---------  
		---------------- 2d ------------------

   PRIVATE
		--------------------------- 2e --------------------------------------
		-------------------- Begin data structures ------------------------- 
      SectionList    : SectionObjList;
      SensorList     : SensorObjList;
      SwitchList     : SwitchObjList;
      TrainList      : TrainObjPtr;
      XMLFilename    : Unbounded_String;
      CurrentSection : SectionObjPtr;
      CurrentSwitch  : SwitchObjPtr;
		-------------------- End data structures ------------------------- 
		-------------------------- 2e ------------------------------------

		
		--------------------------- 2f --------------------------------------
		-------------------- Begin helper function ------------------------- 
		
		procedure disposeSensorNode(ptr : in out sensorNodePtr);
		
		-- Used everywhere 
      PROCEDURE SendToTrainQueue(Cmd : MessageType; Id : Positive);
      PROCEDURE SendToAllTrainQueues(Cmd : MessageType);
		
		-- IdentifyTrain, ReleaseReservation, RepositionTrain
		 PROCEDURE ReleaseBlockings(BlockingList : BlockingObjList);

		-- IdentifyTrain helpers
		function  getTrainsSensorNumbers(trainId : trainIdType) return naturalListType; 
      PROCEDURE AddNewSensorToFront(TrainId : TrainIdType; Sensor : SensorObjPtr);
      FUNCTION  GetSensors (TrainId : TrainIdType) RETURN SensorArrayAccess; 
		procedure getUnbockedUsableSectionsContainingSensor(SensorID : Positive;
                                  FirstSection : OUT SectionObjPtr; SecondSection : OUT SectionObjPtr);
      PROCEDURE GetOccResSections(SensorID : Positive;
                                  FirstSection : OUT SectionObjPtr; SecondSection : OUT SectionObjPtr);
			-- pre none
			-- post 
			--  case 1: both sections occupied/reserved with same trainId
			--          then both pointers /= null
			--  case 2: both sections occupied/reserved with different trainId's
			--          then 
			--  case 2: one section occupied reserved, other reserved
      PROCEDURE RemoveLastSensor(TrainId : TrainIdType);
      function  GetBackSensor(TrainId : TrainIdType) return Positive;
      PROCEDURE GetBackSensor(TrainId : TrainIdType; BackId : OUT Positive);
		procedure flipSensor(sensorPtr : sensorNodePtr); 
		
      -- PositionTrain and build data structures from XML
      PROCEDURE FindSensor(Sensors : SensorObjList; SensorId  : Positive;SensorPtr :OUT SensorNodePtr);
		
		-- PositionTrain and MakeReservation helpers
      FUNCTION  IsSectionUseable(SectionPtr : SectionObjPtr) RETURN Boolean;
		
      -- PositionTrain and AreTrainSensorsLegal helpers
      PROCEDURE FindSection(FirstId : Positive; SecondId : Positive;
                            SectionPtr : OUT SectionNodePtr);
									 
		-- MakeReservation and PositionTrain helpers
      PROCEDURE BlockSections (BlockingList : BlockingObjList);
		
		-- PositionTrain helpers 
      FUNCTION  AllFree(SectList : SectionObjList) RETURN Boolean;
      PROCEDURE FindAllSections(OutSectList : OUT SectionObjList; Sensors : SensorArrayType);
      PROCEDURE PlaceTrainInSections(SectList : SectionObjList; TrainId : TrainIdType);
      PROCEDURE GetSensor(SensorId : Positive; Sensor : OUT SensorObjPtr); 
      function  isNewTrain(TrainId : TrainIdType) return boolean;
		procedure makeEmptySensorObjList(sol : in out sensorObjList);
		procedure updateTrainSensors(TrainId : TrainIdType; Sensors : SensorArrayType);
		PROCEDURE AddNewTrain(TrainId : TrainIdType; Sensors : SensorArrayType);
		
		-- AreTrainSensorsLegal helpers
      FUNCTION IsIn(Sections : SectionObjList; Id: Positive) return boolean;
		
		-- MakeReservation helpers
      PROCEDURE GetFreeSection(SectList : SectionObjList; OutSectPtr : OUT SectionObjPtr);
      PROCEDURE GetTrainSensorList(TrainId : TrainIdType; Sensors : OUT SensorObjList);	
		
      -- MoveSwitch and MoveNextSwitch helpers	
      PROCEDURE GetSections(SwitchPtr  : SwitchObjPtr;
                            ThrownList : OUT SectionObjList; ClosedList : OUT SectionObjList); 
      PROCEDURE GetSection(SectionPtr : OUT SectionNodePtr; 
		                     FrontSensorId : Positive; BackSensorId  :  Positive);
      PROCEDURE SendLoseReservationMessages(SwitchPtr : SwitchNodePtr);
      PROCEDURE FindIdOfTrainLoosingReservation(SwitchPtr : SwitchNodePtr; 
		                                          trainId : out TrainIdType; 
																thereIsReservation : out boolean);		
		PROCEDURE MoveSwitchPossible(SwitchPtr : SwitchNodePtr; Result : OUT Boolean);
      PROCEDURE MoveSwitchPossible(SwitchPtr : SwitchNodePtr; TrainId : TrainIdType; Result : OUT Boolean); 
		-------------------- End helper function   -------------------------  
		-------------------------- 2f --------------------------------------
		
	END LayoutManager;

   TYPE LayoutManagerAccess IS ACCESS LayoutManager;	
	-------------------- End LayoutManager ---------------------------
	--------------------------- 2 ------------------------------------


	-------------------------- 3 -------------------------------------
	-------------------- Begin LayoutTaskType ------------------------
   TASK TYPE LayoutTaskType IS                                    --
      ENTRY SetLayout (L : IN LayoutManagerAccess);               --
   END LayoutTaskType;                                            --
	-------------------- End LayoutTaskType --------------------------
	---------------------------- 3 -----------------------------------

PRIVATE

	---------------------------- 4 -------------------------------------
	-------- Begin definition of types used by LayoutManagerPkg --------  
   TYPE LayoutObj IS TAGGED
      RECORD
         Id : Positive;
      END RECORD;

   TYPE SensorObj IS NEW LayoutObj WITH
      RECORD
         State     : SensorStateType := Open;
         -- StartTime : Time := Clock;
      END RECORD;
      
   TYPE SensorNode IS
      RECORD
         Sensor : SensorObjPtr;
         Next   : SensorNodePtr;
      END RECORD;      
   PROCEDURE disposeBasicSensorNode IS 
      NEW Ada.Unchecked_Deallocation(Object=>SensorNode, Name=>SensorNodePtr);	   
         
   TYPE SensorObjList IS
      RECORD
         Head : SensorNodePtr;
         Tail : SensorNodePtr;
      END RECORD;
      
   TYPE SwitchObj IS NEW LayoutObj WITH
      RECORD
         State         : SwitchStateType := Closed;
         TypeOfSwitch  : ControllerGlobals.SwitchType  := Normal;
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
         State           : SectionStateType    := Free;
         IsUseable       : Boolean             := True;
         SensorList      : SensorObjList;
         SwitchList      : SwitchObjList;
         NextSectionList : SectionObjList;
         PrevSectionList : SectionObjList;
         BlockingList    : BlockingObjList;
         BlockCount      : Natural             := 0;
         TrainId         : TrainIdType := 0;
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
   
   PROCEDURE Free_Section IS
		NEW Ada.Unchecked_Deallocation(Object => SectionObj, Name   => SectionObjPtr);		
	-------- End definition of types used by LayoutManager -------------  
	--------------------------- 4 --------------------------------------
		   

END LayoutPkg;
