with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
with MessageTranslationLibrary; use MessageTranslationLibrary;
with MessageTranslationTypes; use MessageTranslationTypes;
with Ada.Text_IO; use Ada.Text_IO;

procedure TestMessageTrans is
	msg     : MessageType;
begin
   declare
      sensor1 : positive;
      sensor2 : positive;
      flag    : natural;
   begin
      -- Testing doMakeSectionUsable message
      msg := makeDoMakeSectionUsableMsg(100, 200);
      put_line(toEnglish(msg));
      splitDoMakeSectionUsableMsg(msg, sensor1, sensor2);
      if sensor1 /= 100 or sensor2 /= 200 then
         put_line("    FAILED");
         put_line("    sensor1 = " & integer'image(sensor1));
         put_line("    sensor2 = " & integer'image(sensor2));
      end if;

      -- Testing putMakeSectionUsableResponse message
      msg := makePutMakeSectionUsableResponseMsg(100, 200, 1);
      put_line(toEnglish(msg));
      splitPutMakeSectionUsableResponseMsg(msg, sensor1, sensor2, flag);
      if sensor1 /= 100 or sensor2 /= 200 or flag /= 1 then
         put_line("    FAILED");
         put_line("    sensor1 = " & integer'image(sensor1));
         put_line("    sensor2 = " & integer'image(sensor2));
         put_line("    flag    = " & integer'image(flag));
      end if;
   end;

   declare
      iter               : listIteratorType;
      msg                : messageType;
      sensors            : naturalListType;
      sensorCount        : natural;
      PreSensor, FromSensor, ToSensor  : Positive;
      value              : positive;
      slotNum            : SlotType;
      pathKind           : pathType;
   begin
      -- Testing getPath message
      msg := makeGetPathMsg(1, kBreadthFirst, 100, 200, 300);
      put_line(toEnglish(msg));
      splitGetPathMsg(msg, slotNum, pathKind, preSensor, fromSensor, toSensor);
      if preSensor /= 100 or fromSensor /= 200 or toSensor /= 300 then
         put_line("    FAILED sensors = " & integer'image(preSensor) &
                                            integer'image(fromSensor) &
                                            integer'image(toSensor));
      end if;

      -- Testing putPath message
      makeEmpty(sensors);
      for i in 1..6 loop
         addEnd(sensors, 200 + i);
      end loop;
      msg := makePutPathMsg(sensors);
      put_line(toEnglish(msg));
      splitPutPathMsg(msg, sensors);
      sensorCount := getCount(sensors);
      if sensorCount /= 6 then
         put_line("    FAILED sensorCount = " & integer'image(sensorCount));
      end if;
      iter := moveFront(sensors);
      for i in 1..6 loop
	   value := getCurrent(iter);
         if value /= 200 + i then
            put_line("    FAILED sensor" & integer'image(i) & " = " & integer'image(value));
         end if;
         Iter := MoveNext(Iter);
      end loop;
   end;

	new_line;
	put_line("Done");
end;