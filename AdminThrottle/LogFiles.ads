WITH Ada.Calendar; USE Ada.Calendar;
with ada.text_io; use ada.text_io;

package LogFiles is

   procedure initialize(adminLog : boolean; keyboardLog : boolean);
   procedure createKeyboardLog;
   procedure openKeyboardLog;
   procedure closeKeyboardLog;
   procedure putLineKeyboardLog(str : string);
   
   procedure createAdminLog;
   procedure openAdminLog;
   procedure closeAdminLog;
   procedure putLineAdminLog(str : string);

private  
   withAdminLog            : boolean := true;
   withKeyboardLog         : boolean := true;

   adminLog                : file_type;                        -- created in LogFiles.adb
   adminLogFileName        : String := "AdminLog.txt";
   
   KeyboardLog             : File_Type;                        -- created in LogFiles.adb
   keyboardLogFileName     : string := "KeyboardLog.txt";   
   
   kLFString               : string(1..1) := ( 1=> standard.ascii.LF);
   
   StartTime              : Time := Clock;
end LogFiles;