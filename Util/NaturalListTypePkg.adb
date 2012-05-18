with ada.text_io; use ada.text_io;
with ada.exceptions; use ada.exceptions;

package body naturalListTypePkg is

   use naturalListPkg;

   procedure put (str : string; L : naturalListType) is
      iter : listIteratorType;
   begin
     iter := moveFront(L);
     put_line(str & " list count: " & integer'image(getCount(L)));
     while not isNull(iter) loop
        put_line(integer'image(integer(getCurrent(iter))));
        iter := moveNext(iter);
     end loop;
   exception
	   when error : others =>
		   put_line("************ EXCEPTION in globals.put -- " &  Exception_Information (error));
         raise;
   end put;
   
end naturalListTypePkg;
   
