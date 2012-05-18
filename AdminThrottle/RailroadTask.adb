with ada.exceptions; use ada.exceptions;
with ada.text_io; use ada.text_io;

with railroadManager; use railroadManager;
with MessageTranslationTypes; use MessageTranslationTypes;
with TcpIp; use TcpIp;


package body RailroadTask is

   TASK BODY RailroadTaskType IS
      message : messageType;
   BEGIN
      ACCEPT Start;
      begin
         loop
            message := receiveTCPMessageBlocking; 
            objRailroadManager.put(message);
         end loop;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in RailroadTaskType --" & kLFString & Exception_Information (error));
      end;
   END RailroadTaskType;

end;