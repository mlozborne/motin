WITH Ada.Calendar; USE Ada.Calendar;

package MyStringLibrary is

   kLenStr80   : natural := 100;

   subTYPE String80 IS String(1..kLenStr80);

   FUNCTION ToString80(S : String) RETURN String80;
   FUNCTION ToString80(I : Integer) RETURN String80;
   function durationAsString(d :duration) return string;   
private
   kLFString              : string(1..1) := ( 1=> standard.ascii.LF);   
end;

