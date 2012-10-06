WITH Ada.Text_IO; USE Ada.Text_IO; 
with ada.exceptions; use ada.exceptions;
with Ada.Command_Line; use Ada.Command_Line;

WITH ScreenManager;
WITH KeyboardTask; 
WITH RailroadManager; 
with railroadTask; 
with TcpIp; use TcpIp;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with logFiles;
with MessageTranslationTypes;
with Tracer; use Tracer;
with GlobalAdmin; use GlobalAdmin;
with ada.Text_IO; use ada.Text_IO;


PROCEDURE AdminThrottle IS
   kLFString           : string(1..1) := ( 1=> standard.ascii.LF);
   withController      : boolean := true;     
   withAdminLog        : boolean := true;
   withKeyboardLog     : boolean := true;
   IP                  : unbounded_String := to_unbounded_string("0.0.0.0");   
   Port                : unbounded_String := to_unbounded_string("0000");
   layoutFileName      : unbounded_String := to_unbounded_string(""); 
   command             : railroadManager.commandType;
     
BEGIN
   screenManager.objScreenManager.clearTheScreen;
   
   for arg in 1..argument_count loop
      if stringToLower(argument(arg)) = "ip" 
         and then arg + 1 <= argument_count then
         ip := to_unbounded_string(argument(arg+1));
         
      elsif stringToLower(argument(arg)) = "port" 
         and then arg + 1 <= argument_count then
         port := to_unbounded_string(argument(arg+1));
         
      elsif stringToLower(argument(arg)) = "mode" 
         and then arg + 1 <= argument_count then
         withController := (stringToLower(argument(arg+1)) = "controller");
         
      elsif stringToLower(argument(arg)) = "layoutfile" 
         and then arg + 1 <= argument_count then
         layoutFileName := to_unbounded_string(argument(arg+1));
         
      elsif stringToLower(argument(arg)) = "keyboardlog"  
         and then arg + 1 <= argument_count then
         withKeyboardLog := (stringToLower(argument(arg+1)) = "yes");
         
      elsif stringToLower(argument(arg)) = "adminlog" 
         and then arg + 1 <= argument_count then
         withAdminLog := (stringToLower(argument(arg+1)) = "yes");
      end if;
   end loop;
   
   logFiles.initialize(withAdminLog, withKeyboardLog);
   
	initialize39DLL;
	connectToServer(socket, IP, Port, 1.0, true);
	     
   screenManager.objScreenManager.clearTheScreen;
   screenManager.ObjScreenManager.Initialize(withController);
   railroadManager.ObjRailroadManager.Initialize;
   railroadTask.objRailroadTask.start;
   keyboardTask.ObjKeyboardTask.Start(withController);
   
   if layoutFileName /= "" then
		delay 5.0;  -- Don't want to read the XML file before RBLDisplay has had a chance
		            -- to establish itself.
      command.cmd := railroadManager.readXML;
      command.fileName(1..length(layoutFileName)) := to_string(layoutFileName);
      command.fnInUse := length(layoutFileName);
      railroadManager.objRailroadManager.sendMessage(command);
    end if;  

exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in AdminThrottle --" & kLFString & Exception_Information (error));
END AdminThrottle;


