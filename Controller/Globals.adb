with ada.text_io; use ada.text_io;
with Ada.Exceptions; use Ada.Exceptions;

package body globals is

   procedure disposeSensorArray(ptr : in out SensorArrayAccess) is
   begin
      if ptr /= null then
         disposeBasicSensorArray(ptr);
      end if;
   end disposeSensorArray;

	procedure convertSensorArrayToList(A : in sensorArrayAccess; L : in out naturalListType) is
	begin
		makeEmpty(L);
      if A = null then return; end if;
		for i in A.all'range loop
			addEnd(L, A(i));
		end loop;
   EXCEPTION
      WHEN error: OTHERS =>
         Put_Line("************ EXCEPTION Error convertSensorArrayToList:" & Exception_Information(Error));
         RAISE;
	end convertSensorArrayToList;
	
	procedure convertSensorListToArray(L : in naturalListType; A : out sensorArrayAccess) is
		iter : listIteratorType;
	begin
		if getCount(L) = 0 then
			-- A := new SensorArrayType(1..1);
         A := null;
		else
			A := new SensorArrayType(1..getCount(L));
			iter := moveFront(L);
			for i in 1..getCount(L) loop
				A(i) := getCurrent(iter);
				iter := moveNext(iter);
			end loop;			
		end if;
   EXCEPTION
      WHEN error: OTHERS =>
         Put_Line("************ EXCEPTION Error convertSensorListToArray:" & Exception_Information(Error));
         RAISE;
	end convertSensorListToArray;
   
end globals;