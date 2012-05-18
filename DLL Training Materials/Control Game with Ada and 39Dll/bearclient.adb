WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;
WITH Ada.Float_Text_IO; USE Ada.Float_Text_IO;
WITH Ada.Characters.Handling; USE Ada.Characters.Handling;
WITH Screen; USE Screen;

WITH Api39dll; USE Api39dll;

WITH Interfaces.C.Strings;
USE Interfaces.C.Strings;
USE Interfaces;


PROCEDURE BearClient IS
   ConnectSocket       : C.Double;
   IpStrAda            : String := "127.0.0.1";   IpStrC              : Chars_Ptr := New_String(IpStrAda);
   CValue              : C.Double;
   BufferId            : C.Double := C.Double(0.0);
   Char                : Character;
   Direction, KeyValue : Integer;

   ------------------------------------------------------------------------------
   -- Screen manager protected type
   ------------------------------------------------------------------------------

   PROTECTED TYPE ScreenManagerType IS
      PROCEDURE Write (
         Item   : IN String;
         Row    : IN Screen.Depth;
         Column : IN Screen.Width);
   END ScreenManagerType;

   PROTECTED BODY ScreenManagerType IS
      PROCEDURE Write (
            Item   : IN String;
            Row    : IN Screen.Depth;
            Column : IN Screen.Width) IS
      BEGIN
         MoveCursor (Column, Row);
         Put (Item);
      END;
   END ScreenManagerType;

   ScreenManager : ScreenManagerType;   --<<<<<<<<<<<<<<<<<<<<<<<<<<<

   ------------------------------------------------------------------------------
   -- Keyboard input task
   ------------------------------------------------------------------------------

   TASK TYPE KeyboardTaskType IS
      ENTRY Start;
   END;

   TASK BODY KeyboardTaskType IS
      Char  : Character;
   BEGIN
      ACCEPT Start;
      LOOP
         Get_Immediate(Char);
         IF Character'Pos(Char) = 224 THEN -- special key
            Get_Immediate(Char);
         END IF;

         KeyValue := Character'Pos(Char);
         IF KeyValue = 72    THEN -- up arrow
            Direction := 90;
         ELSIF KeyValue = 80 THEN -- down arrow
            Direction := 270;
         ELSIF KeyValue = 75 THEN -- left arrow
            Direction := 180;
         ELSIF KeyValue = 77 THEN -- right arrow
            Direction := 0;
         ELSE
            Direction := -999;
         END IF;

         Cvalue := ClearBuffer(BufferId);
         Cvalue := WriteShort(C.Double(Direction), BufferId);
         Cvalue := SendMessage(
            Sockid => ConnectSocket,
            Ip     => IpStrC,
            Port   => C.Double(14804),
            Buffid => C.Double(0));
      END LOOP;
   END KeyboardTaskType;

   ------------------------------------------------------------------------------
   -- Game input task
   ------------------------------------------------------------------------------

   TASK TYPE GameStatusTaskType IS
      ENTRY Start;
   END;

   TASK BODY GameStatusTaskType IS
      StrCMessage : Chars_Ptr;
   BEGIN
      ACCEPT Start;
      LOOP
         Cvalue := ReceiveMessage(
            Sockid => ConnectSocket,
            Len    => C.Double(0),
            Buffid => C.Double(0));
         StrCMessage := ReadString(Buffid => C.Double(0));
         IF StrCMessage = Null_Ptr THEN
            --put("The string is empty"); new_line;
            ScreenManager.Write ("string is empty             ", 16, 1);
         ELSE
            --put(value(strCMessage)); new_line;
            ScreenManager.Write(Value(StrCMessage), 15, 1);
         END IF;
      END LOOP;
   END ;

   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------

   KeyboardTask   : KeyboardTaskType;
   GameStatusTask : GameStatusTaskType;
BEGIN
   --put("I am the client"); new_line;
   ScreenManager.Write ("I am the client", 1, 1);
   CValue := DllInit;
   ConnectSocket := TcpConnect(
      Ip   => IpStrC,
      Port => C.Double(14804),
      Mode => C.Double(0));
   IF Integer(ConnectSocket) <= 0 THEN
      --put("Start the server first"); new_line;
      ScreenManager.Write("Start the server first", 2, 1);
      RETURN;
   ELSE
      --Put("Connected to server"); New_Line;
      screenManager.write("Connected to the server", 2, 1);
   END IF;

   KeyboardTask.Start;
   GameStatusTask.Start;
END;


