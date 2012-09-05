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
	
	type ArrayOfSensorObjPtrType         is array(positive range <>) of sensorObjPtr;
	type AccessToArrayOfSensorObjPtrType is access ArrayOfSensorObjPtrType;	
   PROCEDURE disposeArrayOfSensorObjPtr IS 
      NEW Ada.Unchecked_Deallocation(Object=>ArrayOfSensorObjPtrType, Name=>AccessToArrayOfSensorObjPtrType);	   
	
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

		PROCEDURE IdentifyTrainV1      (SensorID : Positive);  
			--                                                    Section1   / Section2
			--  case 1: neither section occupied/reserved         null       /  null
			--          random firing
			--          display "C1 MYSTERY SENSOR FIRING not close to any trains: error stop all" 
			--          return
			--  case 2: only one section occupied/reserved        not null   /  null
			--          if section1 occupied and sensor = sn and closed-->open then
			--            display "C2 NORMAL back of train leaving closed sensor sn" 
         --          elsif section1 occupied and sensor = sn and open-->closed then
			--            display "C2 ERROR back of train leaving open sensor. Error stop train"
			--				  error stop train
			--          elsif section1 occupied and sensor = s1 
		   --            diplay"C2 ERROR no reserved section but s1 fired anyway. Error stop train"
			--            this should never happen but if it does
			--				  error stop train
			--				elsif section1 = reserved then
			--            This might happen if s1 didn't fire twice.
			--            We now assume that sensor = sf
			--            if open-->closed then
			--              display "C2 IGNORE front of train approaching sf. Fix when leaving"
			-- 			  else
			--              display "C2 FIXING front of train leaving sf."
			--				    section1 reserved-->occupied
			--              get next free section (nextFreeSection)
			--				    if not found then
			--                 display "C2 ERROR couldn't fix, next section blocked. Error stop train"
			--                 error stop train
			--              else found
			--				       nextFreeSection.state = occupied
			--                 SAFETY set s1 open
			--                 add sf to front of train  (one end of nextFreeSection)
			--                 add sf+1 to front of train (other end of nextFreeSection)
			--                 tell train front sensor has fired
			--                 put train position
			--              end if
			--            end if
			--          else 
			--            display "C2 ERROR no clue what went wrong. Erro stop train"
			--            error stop train
			--          end if
			--          return
			--  case 3: both sections occupied/reserved but with 
			--          different trainId's                       not null   /  not null
			--          if sensor = sn for neither train then
			--            display "C3 ERROR doesn't match back of either train. Error stop both trains."
			--				  error stop both trains
         --          elsif open-->closed 
			--            display "C3 ERROR double occupancy, one train has run into another. Error stop both trains."
         --            error stop both trains	
			--          elsif closed-->open then
			--            display "C3 NORMAL back of train leaving closed sensor sn"
			--          else
			--            display "C3 ERROR no clue what went wrong. Error stop both trains."
			--            error stop both trains
			--          end if
			--          return
			--  case 4: both sections occupied with      
			--          same trainId                              not null   /  not null
			--          if sensor not in (s2..sn-1) then
			--            display "C4 ERROR sensor not under train. Error stop train."
			--			     error stop train
			--          else sensor = sn-1,sn-2, sn-3,..., s2  
			--            sn-1 would be normal, others require fixing
			--            if closed-->open then
			--              display "C4 FIXING sensor unexpectedly closed, flip, and continue"
         --              unexpected outcome, close sensor 
         --              treat normally from here	
			--            end if 
			--            for si in sn..s2 loop
			--              exit when si = sensor
			--              display "C4 removing sensor si from back of train"
			--              open si 
			--              put sensor state message
			--              free section (si-1,si)
			--              put section state message
			--              reduce blocking count for section blocked by (si-1,si)
			--              set section train id to 0
			--              remove si from back of train
			--            	 send back sensor fired message to train
			--            end loop
			--            put train position message
			--            tell all trains to try to move again
			--          end if
			--          return
			-- case 5:  one section occupied, one reserved with
			--          same train id                             not null  /  not null 
			--          if open-->closed then
			--            display "C5 NORMAL front of train approaching sensor s1, ignore"
			--          else front train leaving sensor
			--            display "C5 NORMAL front of train leaving sensor s1"
			--            change reserved section to occupied, tell train, etc...
			--          end if
			--          return


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
		procedure PutTrainPositionMsg(TrainId : trainIdType);
		function  getTrainsSensorNumbers(trainId : trainIdType) return naturalListType; 
      PROCEDURE AddNewSensorToFront(TrainId : TrainIdType; Sensor : SensorObjPtr);
      FUNCTION  GetSensorPtrs (TrainId : TrainIdType) RETURN AccessToArrayOfSensorObjPtrType;
      FUNCTION  GetSensors    (TrainId : TrainIdType) RETURN SensorArrayAccess; 
		procedure getUnbockedUsableSectionsContainingSensor(SensorID : Positive;
                                  FirstSection : OUT SectionObjPtr; SecondSection : OUT SectionObjPtr);
      PROCEDURE GetOccResSections(SensorID : Positive;
                                  FirstSection : OUT SectionObjPtr; SecondSection : OUT SectionObjPtr;
											 searchOutcome : out natural);
			-- pre none
			-- post 
			--  Search all occupied/reserved section that contain sensor and have same trainId
			--                                                    SECTION 1  /  SECTION 2
			--  case 1: neither section occupied/reserved         null       /  null
			--  case 2: only one section occupied/reserved        not null   /  null
			--  case 3: both sections occupied/reserved but with 
			--          different trainId's                       not null   /  null
			--  case 4: both sections occupied with same trainId     
			--                                                    not null   /  not null
			--  case 5: one section occupied, one reserved        not null   /  not null
			--          with same train id
      PROCEDURE RemoveLastSensor(TrainId : TrainIdType);
		function  sensorUnderTrain(trainId : trainIdType; sensor : positive)   return boolean;
		function  sensorIsNextToLast(trainId : trainIdType; sensor : positive) return boolean;
		function  getSensorAtFrontOfReservedSectionPtr(section : sectionObj)   return sensorObjPtr;
      function  GetFrontSensorPtr(TrainId : TrainIdType)                     return sensorObjPtr;
	   function  GetBackSensorPtr(TrainId : TrainIdType)                      return sensorObjPtr;
	   function  getSensorAtFrontOfReservedSection(section : sectionObj)      return positive;
      function  GetFrontSensor(TrainId : TrainIdType)                        return Positive;
      function  GetBackSensor(TrainId : TrainIdType)                         return Positive;
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
