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
with logFiles;

PROCEDURE AdminThrottle IS
   kLFString           : string(1..1) := ( 1=> standard.ascii.LF);
   withController      : boolean := true;     
   withAdminLog        : boolean := true;
   withKeyboardLog     : boolean := true;
   IP                  : unbounded_String := to_unbounded_string("0.0.0.0");   
   Port                : unbounded_String := to_unbounded_string("0000");
   
   function stringToLower (str :string) return string is
      Result : String (Str'Range);
   begin
     for C in  Str'Range loop
        Result (C) := To_Lower (Str (C));
     end loop;
     return Result;
  end stringToLower;
   
BEGIN
   screenManager.objScreenManager.clearTheScreen;
   
   for arg in 1..argument_count loop
      if stringToLower(argument(arg)) = "port" and then arg + 1 <= argument_count then
         port := to_unbounded_string(argument(arg+1));
      elsif stringToLower(argument(arg)) = "ip" and then arg + 1 <= argument_count then
         ip := to_unbounded_string(argument(arg+1));
      elsif stringToLower(argument(arg)) = "standalone" then
         withController := false;
      elsif stringToLower(argument(arg)) = "noadminlog" then
         withAdminLog := false;
      elsif stringToLower(argument(arg)) = "nokeyboardlog" then
         withKeyboardLog := false;
      end if;
   end loop;
   
   logFiles.initialize(withAdminLog, withKeyboardLog);
   
   tcpIp.makeTCPConnection(to_string(IP), to_string(Port));
     
   screenManager.objScreenManager.clearTheScreen;
   screenManager.ObjScreenManager.Initialize(withController);
   railroadManager.ObjRailroadManager.Initialize;
   railroadTask.objRailroadTask.start;
   keyboardTask.ObjKeyboardTask.Start(withController);

exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in AdminThrottle --" & kLFString & Exception_Information (error));
END AdminThrottle;


