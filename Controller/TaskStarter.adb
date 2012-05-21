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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;


PROCEDURE TaskStarter IS
   LayoutPtr : LayoutManagerAccess := NEW LayoutManager;
   Char      : Character;

   function stringToLower (str :string) return string is
      Result : String (Str'Range);
   begin
     for C in  Str'Range loop
        Result (C) := To_Lower (Str (C));
     end loop;
     return Result;
  end stringToLower;
   

BEGIN
   declare
      Port      : unbounded_String := to_unbounded_string("0000");
   begin
      for arg in 1..argument_count loop
         if stringToLower(argument(arg)) = "port" and then arg + 1 <= argument_count then
            port := to_unbounded_string(argument(arg+1));
         elsif stringToLower(argument(arg)) = "ip" and then arg + 1 <= argument_count then
            IpStrAda := to_unbounded_string(argument(arg+1));
         elsif stringToLower(argument(arg)) = "notrace" then
            withTrace := false;
         end if;
      end loop;
      simulator := (port = "1234");
   end;
   
   DECLARE
      LayoutTask : LayoutTaskType;
      SSITask    : SSITaskType;
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
EXCEPTION
   WHEN Error : OTHERS =>
      put_line("**************** EXCEPTION   in TaskStarter: " & Exception_Information(Error));
      myPutLine("Enter any character to end");
      get(char);
END TaskStarter;

