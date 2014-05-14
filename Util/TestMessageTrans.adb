with MessageTranslationLibrary; use MessageTranslationLibrary;
with MessageTranslationTypes; use MessageTranslationTypes;
with Ada.Text_IO; use Ada.Text_IO;

procedure TestMessageTrans is
	msg     : MessageType;
	sensor1 : positive;
	sensor2 : positive;
	flag    : natural;
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
	
	new_line;
	put_line("Done");
end;