--Message Queue Manager Package
--4/8/2011

WITH Interfaces.C;
USE Interfaces.C;
WITH Interfaces; USE Interfaces;
WITH GenericQueue;
WITH GenericList;
WITH ControllerGlobals; USE ControllerGlobals;
with MessageTranslationTypes; use messageTranslationTypes;

PACKAGE CommandQueueManager IS

   SocketListFull: exception;
   LookupTableFull: exception;
   EntryNotFound: exception;
   TrainIdNotFound: exception;

   --put messages incoming to adarail on correct queue
   PROCEDURE put(message : IN MessageType);

   --get first message from outqueue
   PROCEDURE Get(message : OUT MessageType);

   TYPE SocketListType IS ARRAY (0..100) OF C.Double;
   PACKAGE MessageQueue IS NEW GenericQueue(QueueElement=> MessageType);   
   
   --list of sockets used by MessageIO package
   PROTECTED TYPE SocketListT IS
      PROCEDURE AddSocket(Socket: IN C.Double);
      PROCEDURE AddRailroadSocket(Socket: IN C.Double);
      PROCEDURE GetSocketListLength(Length: OUT Integer);
      PROCEDURE GetSocket(Index: IN Integer; Socket: OUT C.Double);
   PRIVATE
      SocketListArray: SocketListType := (OTHERS=>(-1.0));
   END SocketListT;

   PROTECTED TYPE MessageQueueType IS                                       -- mo 12/22/11    
      procedure putMessage(message : IN MessageType);
      entry GetMessage(message : OUT MessageType);
      function IsEmpty return boolean;
   PRIVATE
      Queue: MessageQueue.Queue;
      count : integer := 0;           
   END MessageQueueType;
----------------------------------------------------------

   TYPE LookupEntry IS RECORD
      VirtTrainAddr   : LocoAddressType := LocoAddressType'first;
      VirtSlotNum     : SlotType := slotType'first;
      HasVirtSlot     : Boolean := False;
      PhysTrainAddr   : LocoAddressType := LocoAddressType'first;
      PhysSlotNum     : SlotType := slotType'first;
      HasPhySlot      : Boolean := False;
      InUse           : Boolean := False;
      Sensors         : SensorArrayAccess := null;
   END RECORD;

   TYPE LookUpTableType IS ARRAY (1..KNumTrains) OF LookupEntry;

   --stores virtual slot number and address, physical slot number and address, index into table is train id
   PROTECTED SlotLookupTable IS

      -- display slotLookupTable
      procedure put;
  
      procedure clearEntry(i : natural);
		procedure clearTable;
      PROCEDURE RequestTrainId(TrainId: OUT TrainIdType);
      procedure removeEntryByPhysAddr(PhysAddr : LocoAddressType); 
      procedure removeEntryByTrainId(trainId : trainIdType); 
      PROCEDURE CreateEntry(VirtTrainAddr: IN LocoAddressType;
                            PhysTrainAddr: IN LocoAddressType; 
                            TrainId: IN TrainIdType);
      FUNCTION TrainIdToVirtSlotNum(TrainId: TrainIdType) RETURN  SlotType;
      FUNCTION TrainIdToPhysSlotNum(TrainId: TrainIdType) RETURN SlotType;
      FUNCTION PhysSlotNumToTrainId(PhysSlotNum: SlotType) RETURN TrainIdType;
       
      function addressToTrainId(address : natural) return slotType;  -- mo 1/12/12
      -- return 0 if there is no completed entry in the table containing this address
      --          as either a physical or virtual address
      -- return virtual slot number otherwise
      
      procedure VirtSlotNumToTrainId(VirtSlotNum: SlotType; trainId : out trainIdType; found : out boolean);
      FUNCTION IsPhysAddrInTable(PhysAddr: LocoAddressType) RETURN Boolean;
      FUNCTION PhysAddrToTrainId(PhysAddr: LocoAddressType) RETURN TrainIdType;
      FUNCTION VirtAddrToTrainId(VirtAddr: LocoAddressType) RETURN TrainIdType;
      FUNCTION TrainIdToVirtAddr(TrainId: TrainIdType) RETURN LocoAddressType;
      FUNCTION TrainIdToPhysAddr(TrainId: TrainIdType) RETURN LocoAddressType;
      FUNCTION HasBothSlots(TrainId: TrainIdType) RETURN Boolean;
      PROCEDURE SavePhySlot(PhySlot: SlotType; PhysAddr: LocoAddressType; Result: OUT Boolean);
      PROCEDURE SaveVirtSlot(VirtSlot: SlotType; VirtAddr: LocoAddressType; Result: OUT Boolean);
      PROCEDURE SetTrainSensors(TrainId: TrainIdType; Sensors: SensorArrayType);
      PROCEDURE GetTrainSensors(TrainId: TrainIdType; Sensors: OUT SensorArrayAccess);

   PRIVATE
      LookupTable: LookUpTableType;
   END SlotLookupTable;

   TYPE QueuePtr IS ACCESS MessageQueueType;

   TYPE TrainQueueEntry IS RECORD
      TrainId: TrainIdType;
      TrainQueue: QueuePtr;
   END RECORD;

   PACKAGE Lists IS NEW GenericList(ElementType=>TrainQueueEntry);

   --pairs train IDs with pointers to TrainQueue
   PROTECTED TrainIdQueueList IS
      --add new train to list
      procedure listTrains;
      procedure removeTrain(trainId : trainIdType);
      PROCEDURE AddTrain(TrainId: IN TrainIdType);
      --use TrainId to gain access to TrainQueue
      PROCEDURE GetQueue(TrainId : TrainIdType; TrainQueue : OUT QueuePtr);
      PROCEDURE HasTrain(TrainId: IN TrainIdType; Result : OUT Boolean);

   PRIVATE
      TrainList: Lists.List;
   END TrainIdQueueList;


   SocketList: CommandQueueManager.SocketListT;
   SSIQueue: CommandQueueManager.MessageQueueType;
   LayoutQueue: CommandQueueManager.MessageQueueType;
   OutQueue: CommandQueueManager.MessageQueueType;

END CommandQueueManager;

