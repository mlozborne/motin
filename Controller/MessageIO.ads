--Message IO package
--4/7/2011

PACKAGE MessageIO IS

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

