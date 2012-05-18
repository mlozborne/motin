WITH Ada.Text_IO; USE Ada.Text_IO;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;
WITH Api39dll; USE Api39dll;
WITH Interfaces.C.Strings;
USE Interfaces.C.Strings;
USE Interfaces;

PROCEDURE Server IS
   ListenSocket, AcceptSocket : C.Double;
   Line                       : String(1..10);
   Last                       : Natural;
   CValue                     : C.Double;
   BufferId                   : C.Double := C.Double(0);
   StrAdaGreetings            : String := "Greetings from the server";
   strCGreetings              : chars_ptr := new_string(strAdaGreetings);
BEGIN
   Put("I am the server with address 127.0.0.1 wanting to listen at my port 5000");
   New_Line;

   CValue := DllInit;
   ListenSocket := TcpListen(                          -- Set up a listener that will block
      Port => C.Double(5000),                          --   port
      Max  => C.Double(2),
      Mode => C.Double(0.0));    -- Blocking

   IF Integer(ListenSocket) <= 0 THEN                  -- Did the set up succeed?
      Put("Failed to set up listener on port 5000");
      New_Line;
      RETURN;
   ELSE
      Put("Listening at my port 5000 with my socket"
         & Integer'Image(Integer(ListenSocket)));
      New_Line;
   END IF;

   Put("I am waiting to accept a connection with my socket"
      & Integer'Image(Integer(ListenSocket)));
   New_line;
   AcceptSocket := TcpAccept(                          -- Accept a connection from a client
      Sockid => ListenSocket,                          -- and block until it arrives
      Mode   => C.Double(0)); -- Blocking

   IF Integer(AcceptSocket) <= 0 THEN
      Put("Failed to accept"); New_Line;
      RETURN;
   ELSE
      Put("Accepted connection with my socket"
         & Integer'Image(Integer(AcceptSocket)));
      New_Line;
   END IF;

   CValue := ClearBuffer(BufferId);                   -- Put a message in the buffer

   Cvalue := WriteString(
      Str    => strCGreetings,
      Buffid => C.Double(0));
   CValue := WriteDouble(C.Double(30.0), BufferId);

   Put("I am going to send a message to the client with my socket"
      & Integer'Image(Integer(AcceptSocket)));
   New_Line;

   Cvalue := SendMessage(                             -- Send the message to the client
      Sockid => acceptSocket,
      Ip => New_String(""),                           --   ignored in TCP
      Port => C.Double(0),                            --   ignored in TCP
      Buffid => C.Double(0));                         --   default buffer

   Put("Press ENTER to end");
   new_line;
   Get_Line(Line, Last);

END;

