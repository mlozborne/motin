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
   TYPE SensorNodeList IS PRIVATE;

   TYPE SwitchObj IS PRIVATE;
   TYPE SwitchObjPtr IS ACCESS SwitchObj;
   TYPE SwitchNode IS PRIVATE;
   TYPE SwitchNodePtr IS ACCESS SwitchNode;
   TYPE SwitchNodeList IS PRIVATE;

   type switchStateNode is private;
   type SwitchStateNodePtr is access switchStateNode;
   type switchStateList is private;

   TYPE BlockingNode IS PRIVATE;
   TYPE BlockingNodePtr IS ACCESS BlockingNode;
   TYPE BlockingNodejList IS PRIVATE;

   TYPE SectionObj IS PRIVATE;
   TYPE SectionObjPtr IS ACCESS SectionObj;
   TYPE SectionNode IS PRIVATE;
   TYPE SectionNodePtr IS ACCESS SectionNode;
   TYPE SectionNodeList IS PRIVATE;

   TYPE TrainNode IS PRIVATE;
   TYPE TrainNodePtr IS ACCESS TrainNode;
   
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
      procedure MakeSectionUsable   (sensor1 : positive; sensor2 : positive);
      PROCEDURE MakeReservation      (TrainId :        TrainIdType;
                                      Result  :    OUT Boolean);
      PROCEDURE MoveNextSwitch       (TrainId : TrainIdType;
                                      State   : SwitchStateType);
      PROCEDURE MoveSwitch           (SwitchId : Positive;
                                      State    : SwitchStateType);
      PROCEDURE PositionTrain        (TrainId :        TrainIdType;
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
         --              error stop train
         --          elsif section1 occupied and sensor = s1 
         --            display "C2 ERROR no reserved section but s1 fired anyway. Error stop train"
         --            this should never happen but if it does
         --              error stop train
         --            elsif section1 = reserved then
         --            This might happen if s1 didn't fire twice.
         --            We now assume that sensor = sf
         
         --            if sf closed-->open then
         --              display "C2 ERROR sf closed-->open should have handled this when open-->closed. Error stop train"
         --            else sf open-->closed then
         --                display "C2 Just reached sf, we want to open s1 and handle s1 closed-->open"
         --              change reserved section to occupied, tell train, etc...
         --              if train is now too long then
         --                 display "C5 ERROR train" + trainId + "is too long"
         --                 error stop train
         --              else
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
         --              error stop both trains
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
         --              error stop train
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
         --                send back sensor fired message to train
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
         --            if train is now too long then
         --               display "C5 ERROR train" + trainId + "is too long"
         --               error stop train
         --            else
         --               tell train front sensor has fired
         --               put train position
         --            end if
         --          end if
         --          return
         -- case 6:  both sections reserved with
         --          same train id                             not null  /  not null 
         --          NOT GOING TO HAPPEN SO WILL IGNORE IT COMPLETELY

      -- PROCEDURE IdentifyTrainV2      (SensorID : Positive);  -- sensor oriented, assumes no sensor errors
      PROCEDURE IdentifyTrainV3      (sx : Positive);  -- sensor oriented, assumes sensor errors

      -- End functions to manipulate data structures    
      ------------------------ 2a -------------------------------

      ------------------------------- 2b -----------------------------------------
      -------------------- Begin build data structures from XML ------------------
      -- Creates a new section using private variable CurrentSection
      PROCEDURE bldNewSection (
            Id : Positive);

      -- CurrentSection has been populated with Data
      -- CurrentSection is added to the end of SectionList
      PROCEDURE bldEndSection;

      -- SectionList has been read
      -- Figure out the sections surrounding each section
      PROCEDURE bldEndSectionList;

      -- SwitchList has been read
      -- Figure out what switchs are blocking sections
      --   based on current state
      PROCEDURE bldEndSwitchList;

      -- Adds a new switch to CurrentSection's SwitchList
      -- If the switch is not already in the Full SwitchList, it is added
      PROCEDURE bldAddSwitch (
            Id    : Positive;
            state :  switchStateType);

      -- Find the Switch with Id
      -- private variable CurrentSwitch points to the switch
      -- set the type of the switch
      PROCEDURE bldUpdateSwitch (
            Id           : Positive;
            TypeOfSwitch : ControllerGlobals.SwitchType;
            state        : switchStateType);

      -- Add a pointer to the sensor with Id = NarrowId to the list of narrow
      --   sensors in CurrentSwitch
      PROCEDURE bldUpdateSwitchNarrow (
            NarrowId : Positive);

      -- Add a pointer to the sensor with Id = ClosedId to the list of closed
      --    sensors in CurrentSwitch
      PROCEDURE bldUpdateSwitchClosed (
            ClosedId : Positive);

      -- Add a pointer to the sensor with Id = ThrownId to CurrentSwitch
      PROCEDURE bldUpdateSwitchThrown (
            ThrownId : Positive);

      -- Adds a new sensor to CurrentSection's SensorList
      -- If the sensor is not already in the Full SensorList, it is added
      PROCEDURE bldAddSensor (
            Id : Positive);

      -- Adds Id to CurrentSection's BlockingList
      PROCEDURE bldAddBlocking (
            Id : Positive);
      -------------------- End build data structures from XML --------------------
      ------------------------------ 2b ------------------------------------------


      ------------------------------- 2c ----------------------------------
      ----------------- Begin debug print data structures -----------------
      PROCEDURE Print_Sections (
            Sections    : SectionNodeList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean        := False);
      PROCEDURE Print_Sensors (
            Sensors     : SensorNodeList;
            Indent      : Natural;
            Output      : File_Type;
            PrintOnlyId : Boolean       := False);
      PROCEDURE Print_Switchs (
            Switchs     : SwitchNodeList;
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
      SectionList    : SectionNodeList;
      SensorList     : SensorNodeList;
      SwitchList     : SwitchNodeList;
      TrainList      : TrainNodePtr;
      XMLFilename    : Unbounded_String;
      CurrentSection : SectionObjPtr;
      CurrentSwitch  : SwitchObjPtr;
      -------------------- End data structures ------------------------- 
      -------------------------- 2e ------------------------------------

      
      --------------------------- 2f --------------------------------------
      -------------------- Begin helper function ------------------------- 
      
      procedure disposeSensorNode(ptr : in out sensorNodePtr);
      procedure disposeSectionNode(ptr : in out sectionNodePtr);
      
      -- Used everywhere 
      PROCEDURE SendToTrainQueue(Cmd : MessageType; Id : Positive);
      PROCEDURE SendToAllTrainQueues(Cmd : MessageType);
      
      -- IdentifyTrain, ReleaseReservation, RepositionTrain
       PROCEDURE ReleaseBlockings(BlockingList : BlockingNodejList);

      -- IdentifyTrain helpers
      procedure PutTrainPositionMsg(TrainId : trainIdType);
      function  getTrainsSensorNumbers(trainId : trainIdType) return naturalListType; 
      PROCEDURE AddNewSensorToFront(TrainId : TrainIdType; Sensor : SensorObjPtr);
      FUNCTION  GetSensorPtrs (TrainId : TrainIdType) RETURN AccessToArrayOfSensorObjPtrType;
      FUNCTION  GetSensors    (TrainId : TrainIdType) RETURN SensorArrayAccess; 
      function  countSensors(trainId : trainIdType) return natural;
      procedure getUnblockedUsableSectionsContainingSensor(
                                  SensorID : Positive;
                                  FirstSection : OUT SectionObjPtr; 
                                  SecondSection : OUT SectionObjPtr);
      PROCEDURE GetOccResSections(SensorID : Positive;
                                  FirstSection : OUT SectionObjPtr; 
                                  SecondSection : OUT SectionObjPtr;
                                  searchOutcome : out natural);
         -- pre none
         -- post 
         --  Search all occupied/reserved section that contain sensor
         --                                                    SECTION 1  /  SECTION 2
         --  case 1: neither section occupied/reserved         null       /  null
         --  case 2: only one section occupied/reserved        not null   /  null
         --  case 3: both sections occupied/reserved but with 
         --          different trainId's                       not null   /  not null
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
      
      --vvvvvvvvvvvvvvvvvvvvvvvvvvvv Helper Functions for IdentifyTrainV3 vvvvvvvvvvvvvvvvvvvvvv
      procedure errorStopTrainsAtBadSensor(idSensorCase         : positive;
                                           sx                   : positive;
                                           leftSectionPtr       : sectionObjPtr;
                                           rightSectionPtr      : sectionObjPtr);
      function trainInSection(trainId : trainIdtype; sp : sectionObjPtr) return boolean;
      function sectionReserved(sp : sectionObjPtr) return boolean;
      function sectionOccupied(sp : sectionObjPtr) return boolean;
      procedure identifySensor(sx               : positive;               -- MO March 2014
                               idSensorCase     : out positive;
                               expectationError : out boolean;
                               trainId          : out trainIdType;
                               leftSectionPtr   : out sectionObjPtr;
                               rightSectionPtr  : out sectionObjPtr);
         -- We assume here that the train of interest is moving from right to left.
         -- Case 1: sx not in controller’s sensor list
         -- Case 2: sx = sn (even if sx = t1 or sx=tf for another train)
         --         expecting O* for (sn-1,sx,-)=(sn-1,sn,-) where
         --         O belongs to this train  
         --         * belongs to another train or no train
         --         This normally happens when the back of the train leaves a closed sensor
         -- Case 3: sx=si in <s2,...,sn-1>, where n  > 2
         --         Expecting O...O for (si-1,sx,...,sn-1,sn)=(si-1,si,...,sn-1,sn) where
         --         all O belongs to this train
         --         We don't attempt to determine the train's direction
         --         If sx = sn-1 and open-->closed then everything is normal.
         --         If sx = sn-2,sn-3,... then this means that the back of the train passed over
         --         one or more sensors without firing them. If the front of the train fired all sensors correctly
         --         then the sensor is going from open-->closed otherwise it could be going from closed-->open
         -- Case 4: sx = s1 (and by Case 2 sx/=tn for all other trains)
         --           expecting RO for (sf,sx,s2)=(sf,s1,s2) where
         --           R and O belong to this train
         -- Case 5: sx = sf (and by the previous cases, sx belongs to no other train)
         --           expecting notRR for (-,sx,s1)=(sf+1,sf,s1) where
         --           R belongs to this train
         --           notR belongs to another train or no train
         -- Case 6: other, such as the sensor is not associated with any train
         --         or both sections are reserved.
      --^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      
      -- PositionTrain and build data structures from XML
      PROCEDURE FindSensor(Sensors : SensorNodeList; SensorId  : Positive; SensorPtr :OUT SensorNodePtr);
      
      -- PositionTrain and MakeReservation helpers
      FUNCTION  IsSectionusable(SectionPtr : SectionObjPtr) RETURN Boolean;
      
      -- PositionTrain and AreTrainSensorsLegal helpers
      PROCEDURE FindSection(FirstId : Positive; SecondId : Positive;
                            SectionPtr : OUT SectionNodePtr);
                            
      -- MakeReservation and PositionTrain helpers
      PROCEDURE BlockSections (BlockingList : BlockingNodejList);
      
      -- PositionTrain helpers 
      FUNCTION  AllFree(SectList : SectionNodeList) RETURN Boolean;
      PROCEDURE FindSectionsCorrespondingToListOfSensors(OutSectList : OUT SectionNodeList; Sensors : SensorArrayType);
      PROCEDURE PlaceTrainInSections(SectList : SectionNodeList; TrainId : TrainIdType);
      PROCEDURE GetSensor(SensorId : Positive; Sensor : OUT SensorObjPtr); 
      function  isNewTrain(TrainId : TrainIdType) return boolean;
      procedure makeEmptySensorNodeList(snl : in out SensorNodeList);
      procedure makeEmptySectionNodeList(snl : in out SectionNodeList);
      procedure updateTrainSensors(TrainId : TrainIdType; Sensors : SensorArrayType);
      PROCEDURE AddNewTrain(TrainId : TrainIdType; Sensors : SensorArrayType);
      
      -- AreTrainSensorsLegal helpers
      FUNCTION IsIn(Sections : SectionNodeList; Id: Positive) return boolean;
      
      -- MakeReservation helpers
      PROCEDURE GetFreeSection(SectList : SectionNodeList; OutSectPtr : OUT SectionObjPtr);
      PROCEDURE GetTrainSensorList(TrainId : TrainIdType; Sensors : OUT SensorNodeList);   
      
      -- MoveSwitch and MoveNextSwitch helpers   
      PROCEDURE bldGetSectionsContainingSwitch(SwitchPtr  : SwitchObjPtr;
                            ThrownList : OUT SectionNodeList; ClosedList : OUT SectionNodeList); 
      PROCEDURE GetSection(SectionPtr : OUT SectionNodePtr; 
                           FrontSensorId : Positive; BackSensorId  :  Positive);
      PROCEDURE SendLoseReservationMessages(SwitchPtr : SwitchObjPtr);
      PROCEDURE FindIdOfTrainLoosingReservation(SwitchPtr : SwitchNodePtr; 
                                                trainId : out TrainIdType; 
                                                thereIsReservation : out boolean);      
      PROCEDURE MoveSwitchPossible(SwitchPtr : SwitchObjPtr; Result : OUT Boolean);
      PROCEDURE MoveSwitchPossible(SwitchPtr : SwitchObjPtr; TrainId : TrainIdType; Result : OUT Boolean); 
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
   TYPE LayoutObj IS TAGGED RECORD
      Id : Positive;
   END RECORD;
   
   -------------------------------------------
   
   TYPE SensorObj IS NEW LayoutObj WITH RECORD
      State     : SensorStateType := Open;
      -- StartTime : Time := Clock;
   END RECORD;
      
   TYPE SensorNode IS RECORD
      Sensor : SensorObjPtr := null;
      Next   : SensorNodePtr := null;
   END RECORD;  
   
   PROCEDURE disposeBasicSensorNode IS 
      NEW Ada.Unchecked_Deallocation(Object=>SensorNode, Name=>SensorNodePtr);      
         
   TYPE SensorNodeList IS RECORD
      Head : SensorNodePtr := null;
      Tail : SensorNodePtr := null;
   END RECORD;
      
-------------------------------------------

   TYPE SwitchObj IS NEW LayoutObj WITH RECORD
      State         : SwitchStateType := Closed;
      TypeOfSwitch  : ControllerGlobals.SwitchType  := Normal;
      ClosedSensors : SensorNodeList;
      NarrowSensors : SensorNodeList;
      ThrownSensor  : SensorObjPtr := null;    --x To generalize make this a list
      SectionNList  : SectionNodeList;   --x all sections containing this switch
   END RECORD;
      
   TYPE SwitchNode IS RECORD
      Switch : SwitchObjPtr := null;
      Next   : SwitchNodePtr := null;
   END RECORD;
      
   TYPE SwitchNodeList IS RECORD
      Head : SwitchNodePtr := null;
      Tail : SwitchNodePtr := null;
   END RECORD;

-------------------------------------------

   type switchStateNode is record
      switch      : switchObjPtr := null;
      myState     : switchStateType;
      next        : SwitchStateNodePtr := null;
   end record;

   type switchStateList is record
      Head : SwitchStateNodePtr := null;
      Tail : SwitchStateNodePtr := null;
    end record;
    
-------------------------------------------

   TYPE BlockingNode IS NEW LayoutObj WITH RECORD
         Next : BlockingNodePtr := null;
   END RECORD;
      
   TYPE BlockingNodejList IS RECORD
         Head : BlockingNodePtr := null;
         Tail : BlockingNodePtr := null;
   END RECORD;

-------------------------------------------

   TYPE SectionObj IS NEW LayoutObj WITH RECORD
      State             : SectionStateType    := Free;
      Isusable          : Boolean             := True;
      SensorList        : SensorNodeList;    --x replace this with firstSensor and secondSensor
      --x SwitchList        : SwitchNodeList;
      mySwitchStateList : switchStateList;
      NextSectionList   : SectionNodeList;
      PrevSectionList   : SectionNodeList;
      BlockingList      : BlockingNodejList;
      BlockCount        : Natural             := 0;
      TrainId           : TrainIdType         := 0;
   END RECORD;
      
   TYPE SectionNode IS RECORD
      Section : SectionObjPtr := null;
      Next    : SectionNodePtr := null;
   END RECORD;
      
   PROCEDURE disposeBasicSectionNode IS 
      NEW Ada.Unchecked_Deallocation(Object=>SectionNode, Name=>SectionNodePtr); 
      
    TYPE SectionNodeList IS RECORD
      Head : SectionNodePtr := null;
      Tail : SectionNodePtr := null;
   END RECORD;

-------------------------------------------

   TYPE TrainNode IS RECORD
      TrainId     : TrainIdType;
      SensorCount : Positive;
      SensorList  : SensorNodeList;
      Queue       : CommandQueueManager.QueuePtr;
      Next        : TrainNodePtr := null;
   END RECORD;

-------------------------------------------

   PROCEDURE disposeBasicTrainNode IS 
      NEW Ada.Unchecked_Deallocation(Object=>TrainNode, Name=>TrainNodePtr);      
   
   PROCEDURE Free_Section IS
      NEW Ada.Unchecked_Deallocation(Object => SectionObj, Name   => SectionObjPtr);   
      
   -------- End definition of types used by LayoutManager -------------  
   --------------------------- 4 --------------------------------------
         

END LayoutPkg;
