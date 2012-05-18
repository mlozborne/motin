WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;
WITH Ada.Float_Text_IO; USE Ada.Float_Text_IO;
WITH Api39dll; USE Api39dll;
WITH Interfaces.C.Strings;
USE Interfaces.C.Strings;
USE Interfaces;


PROCEDURE Client IS
   ConnectSocket : C.Double;
   Line          : String(1..10);
   Last          : Natural;
   CValue        : C.Double;
   BufferId      : C.Double := C.Double(0.0);
   StrCGreetings : Chars_Ptr;
BEGIN
   Put("I am the client wanting to connect to server 127.0.0.1 port 5000");
   New_Line;

   CValue := DllInit;
   loop
      ConnectSocket := TcpConnect(                              -- Try to connect to server
         Ip => New_String("127.0.0.1"),                         --   on host 127.0.0.1
         Port => C.Double(5000),                                --   at port 5000
         Mode => C.Double(0));                                  --   blocking connection
      EXIT WHEN integer(ConnectSocket) > 0;
   end loop;

   Put("Connected to server with my socket"
       & Integer'Image(Integer(ConnectSocket)));
   New_Line;

   Put("Waiting to receive a message from the server using my socket"
      & Integer'Image(Integer(ConnectSocket)));
   New_Line;

   Cvalue := ReceiveMessage(                                 -- Receive a message from the server
      Sockid => ConnectSocket,                               -- and block until response
      Len => C.Double(0),
      Buffid => C.Double(0));

   StrCGreetings := ReadString(Buffid => C.Double(0));       -- Unpack the message from a buffer
   CValue := ReadDouble(BufferId);

   IF StrCGreetings = Null_Ptr THEN                          -- Display the message
      Put ("string is empty"); New_Line;
   ELSE
      Put (Value(StrCGreetings)); New_Line;
   END IF;
   Put(Float(Cvalue)); New_Line;

   Put("Press ENTER to end"); New_Line;
   Get_Line(Line, Last);
END;


