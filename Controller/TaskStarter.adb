WITH Ada.Text_IO; USE Ada.Text_IO;
WITH MessageIO; USE MessageIO;
WITH Interfaces; USE Interfaces;
WITH MessageTranslationLibrary; USE MessageTranslationLibrary;
WITH Globals; USE Globals;
WITH LayoutPkg; USE LayoutPkg;
WITH SSIPkg; USE SSIPkg;
WITH LocoBuffer; USE LocoBuffer;
with Ada.Text_IO, Ada.Exceptions;
USE Ada.Text_IO, Ada.Exceptions;

PROCEDURE TaskStarter IS
   LayoutPtr : LayoutManagerAccess := NEW LayoutManager;
   Char      : Character;

BEGIN
   -- Put("Enter 's' for simulator or 'r' for railroad: ");
   -- Get(Char);
   -- IF Char = 's' THEN
      -- Simulator := True;
   -- END IF;

   -- Put_Line("TaskStart pkg in TaskStarter: starting tasks");
   
   -- simulator := true;

   DECLARE
      LayoutTask : LayoutTaskType;
      SSITask    : SSITaskType;
      --MessageIO package
      ListenForThrottleTask              : ListenForThrottleTaskType;
      ConnectToSimulatorOrLocoBufferTask : ConnectToSimulatorOrLocoBufferTaskType;
      SendMessageTask                    : SendMessageTaskType;
      ReceiveMessageTask                 : ReceiveMessageTaskType;
   BEGIN
      LayoutTask.SetLayout(LayoutPtr);
      SSITask.SetLayout(LayoutPtr);
      IF NOT Simulator THEN
         DECLARE
            ListenForLocoBufferClientsTask       : ListenForLocoBufferClientsTaskType;
            WriteLocoBufferStringTask            : WriteLocoBufferStringTaskType;
            ReadLocoBufferByteTask               : ReadLocoBufferByteTaskType;
         BEGIN
            NULL;
         END;
      END IF;
   END;
   -- put_line("Enter any character to end");
   -- get(char);
EXCEPTION
   WHEN Error : OTHERS =>
      Put_Line("**************** EXCEPTION   in TaskStarter: " & Exception_Information(Error));
      put_line("Enter any character to end");
      get(char);
END TaskStarter;

