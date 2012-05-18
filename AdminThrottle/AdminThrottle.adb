WITH Ada.Text_IO; USE Ada.Text_IO;
with ada.exceptions; use ada.exceptions;

WITH ScreenManager;
WITH KeyboardTask; 
WITH RailroadManager; 
with railroadTask; 
with TcpIp; 
with logFiles;

PROCEDURE AdminThrottle IS
   kLFString           : string(1..1) := ( 1=> standard.ascii.LF);
   inControllerMode    : boolean := false;             
   
BEGIN
   -- logFiles.createKeyboardLog;
   -- logFiles.createAdminLog;
   
   screenManager.objScreenManager.clearTheScreen;
   inControllerMode := tcpIp.makeTCPConnection;
   screenManager.objScreenManager.clearTheScreen;
   screenManager.ObjScreenManager.Initialize(inControllerMode);
   railroadManager.ObjRailroadManager.Initialize;
   railroadTask.objRailroadTask.start;
   keyboardTask.ObjKeyboardTask.Start(inControllerMode);

exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in AdminThrottle --" & kLFString & Exception_Information (error));
END AdminThrottle;


