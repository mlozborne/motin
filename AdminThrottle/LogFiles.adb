with ada.exceptions; use ada.exceptions;

package body LogFiles is

   procedure createKeyboardLog is
   begin
      Create(File => keyboardLog, Mode => Out_File, Name => keyboardLogFileName);
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.CreateKeyboardLog --" & 
                      kLFString & Exception_Information (error));
   end createKeyboardLog;
   
   procedure createAdminLog is
   begin
      Create(File => adminLog, Mode => Out_File, Name => adminLogFileName);
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.createAdminLog --" & 
                      kLFString & Exception_Information (error));
   end createAdminLog;
   
-----------------------------------------------------------------------------
   
   procedure openAdminLog is 
   begin
      if not is_open(adminLog) then
         open(adminLog, append_file, adminLogFileName);
      end if;
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.openAdminLog --" & 
                      kLFString & Exception_Information (error));
   end openAdminLog;
   
   procedure openKeyboardLog is 
   begin
      if not is_open(keyboardLog) then
         open(keyboardLog, append_file, keyboardLogFileName);
      end if;
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.openKeyboardLog --" & 
                      kLFString & Exception_Information (error));
   end openKeyboardLog;
   
-----------------------------------------------------------------------------

   procedure closeAdminLog is
   begin
      if is_open(adminLog) then
         close(adminLog);
      end if;
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.closeAdminLog --" & 
                      kLFString & Exception_Information (error));
   end closeAdminLog;
   
   procedure closeKeyboardLog is
   begin
      if is_open(keyboardLog) then
         close(keyboardLog);
      end if;
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.closeKeyboardLog --" & 
                      kLFString & Exception_Information (error));
   end closeKeyboardLog;
   
-----------------------------------------------------------------------------

   procedure putLineKeyboardLog(str : string) is
   begin
      if is_open(keyboardLog) then
         Put_Line(keyboardLog, "*" & natural'image(natural(clock - startTime)) & " " & str);
      end if;
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.putLineKeyboardLog --" & 
                      kLFString & Exception_Information (error));
   end putLineKeyboardLog;
   
   procedure putLineAdminLog(str : string) is
   begin 
      if is_open(adminLog) then
         Put_Line(AdminLog, natural'image(natural(clock - startTime)) & " " & str);
      end if;
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.putLineAdminLog --" & 
                      kLFString & Exception_Information (error));
   end putLineAdminLog;
   
-----------------------------------------------------------------------------

begin
   createKeyboardLog;
   createAdminLog;
end LogFiles;