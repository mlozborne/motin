--Message IO package
--4/7/2011
with MessageTranslationTypes; use MessageTranslationTypes;
WITH Ada.Calendar; USE Ada.Calendar;

PACKAGE MessageIO IS

   -- Support comparison of last message written to simulator/locobuffer with most
	-- recent message read from simulator/locobuffer.
	protected type LastMessageManagerType is
		procedure saveMessage(msg : messageType);
		procedure isThisAnEcho(msg : messageType; echo : out boolean);
	private
		lastMessageSentToRailroad : messageType;
		kEmptyMessage             : messageType;
	end LastMessageManagerType;
	
	lastMessageManager : LastMessageManagerType;
	
	ignoreMessagesUntilTime   : Time := clock;
	ignoreDurationForPowerOn  : duration := 60.0;
	ignoreDurationForPowerOff : duration := 10.0;

   --continuously waits for throttle connections
   --stores in socketlist
   TASK TYPE ListenForThrottleTaskType IS
   END ListenForThrottleTaskType;

   --connects to LocoBuffer server
   --stores in socketlist, end task
   TASK TYPE ConnectToSimulatorOrLocoBufferTaskType IS
   END ConnectToSimulatorOrLocoBufferTaskType;

   --get message from out queue
   --internal message to LocoNet message
   --send message to server
   TASK TYPE SendMessageTaskType IS
   END SendMessageTaskType;

   --Receive message from socket
   --LocoNet message to internal message
   --put message into in queue
   TASK TYPE ReceiveMessageTaskType IS
   END ReceiveMessageTaskType;


END MessageIO;

