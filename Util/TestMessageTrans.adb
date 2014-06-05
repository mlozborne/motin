with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
with MessageTranslationLibrary; use MessageTranslationLibrary;
with MessageTranslationTypes; use MessageTranslationTypes;
with Ada.Text_IO; use Ada.Text_IO;

procedure TestMessageTrans is 
	msg     : MessageType;
	sensor1 : positive;
	sensor2 : positive;
	flag    : natural;
   sensors : naturalListType;
begin
	msg := makeDoMakeSectionUseableMsg(100, 200);
	splitDoMakeSectionUseableMsg(msg, sensor1, sensor2);
	if sensor1 /= 100 or sensor2 /= 200 then
		put_line("makeDoMakeSectionUseableMsg/splitDoMakeSectionUseableMsg failed");
		put_line(toEnglish(msg));
		put_line("sensor1 = " & integer'image(sensor1));
		put_line("sensor2 = " & integer'image(sensor2));
	end if;
	
	msg := makePutMakeSectionUseableResponseMsg(100, 200, 1);
	splitPutMakeSectionUseableResponseMsg(msg, sensor1, sensor2, flag);
	if sensor1 /= 100 or sensor2 /= 200 or flag /= 1 then
		put_line("makePutMakeSectionUseableResponseMsg/splitPutMakeSectionUseableResponseMsg failed");
		put_line(toEnglish(msg));
		put_line("sensor1 = " & integer'image(sensor1));
		put_line("sensor2 = " & integer'image(sensor2));
		put_line("flag    = " & integer'image(flag));
	end if;	
   
   makeEmpty(sensors);
	for i in 1..6 loop		
	   addEnd(sensors, 200 + i);
   end loop;
   msg := makePutPathMsg(sensors);
   put_line("makePutPathMsg expecting to see the numbers 201..2006");
   put_line("     " & toEnglish(msg));
	
	new_line;
	put_line("Done");
end;