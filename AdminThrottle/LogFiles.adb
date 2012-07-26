with ada.exceptions; use ada.exceptions;

package body LogFiles is

   procedure initialize(adminLog : boolean; keyboardLog : boolean) is 
   begin
      withAdminLog := adminLog;
      withKeyboardLog := keyboardLog;
      if withKeyboardLog then
         createKeyboardLog;
      end if;
      if withAdminLog then
         createAdminLog;
      end if;
   exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in LogFiles.createAdminLog --" & 
                   kLFString & Exception_Information (error));
   end initialize;
-----------------------------------------------------------------------------

   procedure createKeyboardLog is
   begin
      if not withKeyboardLog then
         return;
      end if;
      Create(File => keyboardLog, Mode => Out_File, Name => keyboardLogFileName);
   exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in LogFiles.CreateKeyboardLog --" & 
                      kLFString & Exception_Information (error));
   end createKeyboardLog;
   
   procedure createAdminLog is
   begin
      if not withAdminLog then
         return;
      end if;
      Create(File => adminLog, Mode => Out_File, Name => adminLogFileName);
   exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in LogFiles.createAdminLog --" & 
                   kLFString & Exception_Information (error));
   end createAdminLog;
   
-----------------------------------------------------------------------------
   
   procedure openAdminLog is 
   begin
      if not withAdminLog then
         return;
      end if;
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
      if not withKeyboardLog then
         return;
      end if;
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
      if not withAdminLog then
         return;
      end if;
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
      if not withKeyboardLog then
         return;
      end if;
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
      if not withKeyboardLog then
         return;
      end if;
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
      if not withAdminLog then
         return;
      end if;
      if is_open(adminLog) then
         Put_Line(AdminLog, natural'image(natural(clock - startTime)) & " " & str);
      end if;
   exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in LogFiles.putLineAdminLog --" & 
                   kLFString & Exception_Information (error));
   end putLineAdminLog;
	
--------------------------------------------------------------------------

	function  adminLoggingOn return boolean is
	begin	
		return withAdminLog;
   exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in LogFiles.adminLoggingOn --" & 
                   kLFString & Exception_Information (error));
		   return false;
	end adminLoggingOn; 
   
end LogFiles;