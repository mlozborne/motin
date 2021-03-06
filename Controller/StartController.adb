WITH Ada.Text_IO; USE Ada.Text_IO;
WITH MessageIO; USE MessageIO;
WITH Interfaces; USE Interfaces;
WITH MessageTranslationLibrary; USE MessageTranslationLibrary;
WITH ControllerGlobals; USE ControllerGlobals;
WITH LayoutPkg; USE LayoutPkg;
WITH SSIPkg; USE SSIPkg;
--WITH LocoBuffer; USE LocoBuffer;
with Ada.Text_IO, Ada.Exceptions;
USE Ada.Text_IO, Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with Tracer; use Tracer;


PROCEDURE StartController IS
   LayoutPtr : LayoutManagerAccess := NEW LayoutManager;
   Char      : Character;

BEGIN
   declare
      Port      : unbounded_String := to_unbounded_string("0000");
   begin
      for arg in 1..argument_count loop
         if stringToLower(argument(arg)) = "port" 
            and then arg + 1 <= argument_count then
            port := to_unbounded_string(argument(arg+1));
            
         elsif stringToLower(argument(arg)) = "ip" 
            and then arg + 1 <= argument_count then
            IpStrAda := to_unbounded_string(argument(arg+1));
            
         elsif stringToLower(argument(arg)) = "trace" 
            and then arg + 1 <= argument_count then
            withTrace := (stringToLower(argument(arg+1)) = "yes");
         end if;
      end loop;
      simulator := (port = "1234");  -- global variable used by ConnectToSimulatorOrLocoBufferTaskType
   end;

   DECLARE
      LayoutTask                         : LayoutTaskType;
      SSITask                            : SSITaskType;
      ListenForThrottleTask              : ListenForThrottleTaskType;
      ConnectToSimulatorOrLocoBufferTask : ConnectToSimulatorOrLocoBufferTaskType;
      SendMessageTask                    : SendMessageTaskType;
      ReceiveMessageTask                 : ReceiveMessageTaskType;
   BEGIN
      LayoutTask.SetLayout(LayoutPtr);
      SSITask.SetLayout(LayoutPtr);
   END;
EXCEPTION
   WHEN Error : OTHERS =>
      put_line("**************** EXCEPTION   in TaskStarter: " & Exception_Information(Error));
      myPutLine("Enter any character to end");
      get(char);
END StartController;

