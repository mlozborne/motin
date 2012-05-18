with ada.text_IO; use ada.text_IO;
with ada.exceptions; use ada.exceptions;

package body MyStringLibrary is

      FUNCTION ToString80(S : String) RETURN String80 IS
         MyString : String80 := (OTHERS => ' ');
         len      : natural := s'length; 
      BEGIN
         if len > kLenStr80 then len := kLenStr80; end if;
         MyString(1..len) := s(s'first..len);
         RETURN MyString;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in MyStringLibrary.ToString80 --" & kLFString & Exception_Information (error));
            raise;
      END ToString80;

      FUNCTION ToString80(I : Integer) RETURN String80 IS
      BEGIN
         RETURN ToString80(Integer'Image(I));
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in MyStringLibrary.ToString80 --" & kLFString & Exception_Information (error));
            raise;
      END ToString80;

      function durationAsString(d :duration) return string is
      begin
         return toString80(Natural'Image(natural(d)))(1..6);
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in MyStringLibrary.durationAsString --" & kLFString & Exception_Information (error));
            raise;
      end durationAsString;
      
end MyStringLibrary;