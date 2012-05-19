WITH Ada.Text_IO; USE Ada.Text_IO;
with ada.exceptions; use ada.exceptions;
with Ada.Command_Line; use Ada.Command_Line;

WITH ScreenManager;
WITH KeyboardTask; 
WITH RailroadManager; 
with railroadTask; 
with TcpIp; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
-- with logFiles;

PROCEDURE AdminThrottle IS
   kLFString           : string(1..1) := ( 1=> standard.ascii.LF);
   inControllerMode    : boolean := true;     
   IP                  : unbounded_String := to_unbounded_string("127.0.0.1");   
   Port                : unbounded_String := to_unbounded_string("1235");
   
   function stringToLower (str :string) return string is
      Result : String (Str'Range);
   begin
     for C in  Str'Range loop
        Result (C) := To_Lower (Str (C));
     end loop;
     return Result;
  end stringToLower;
   
BEGIN
   -- logFiles.createKeyboardLog;
   -- logFiles.createAdminLog;
   
   screenManager.objScreenManager.clearTheScreen;
   
   for arg in 1..argument_count loop
      if stringToLower(argument(arg)) = "port" then
         port := to_unbounded_string(argument(arg+1));
      elsif stringToLower(argument(arg)) = "ip" then
         ip := to_unbounded_string(argument(arg+1));
      end if;
   end loop;
   
   tcpIp.makeTCPConnection(to_string(IP), to_string(Port));
   
   
   screenManager.objScreenManager.clearTheScreen;
   screenManager.ObjScreenManager.Initialize(inControllerMode);
   railroadManager.ObjRailroadManager.Initialize;
   railroadTask.objRailroadTask.start;
   keyboardTask.ObjKeyboardTask.Start(inControllerMode);

exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in AdminThrottle --" & kLFString & Exception_Information (error));
END AdminThrottle;


