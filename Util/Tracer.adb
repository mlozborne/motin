with ada.text_io; use ada.text_io;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Tracer is

   procedure myPutLine(str : string) is
   begin
      if withTrace then
         put_line(str);
      end if;
   end myPutLine;

   function stringToLower (str :string) return string is
      Result : String (Str'Range);
   begin
     for C in  Str'Range loop
        Result (C) := To_Lower (Str (C));
     end loop;
     return Result;
  end stringToLower;
  
end Tracer;