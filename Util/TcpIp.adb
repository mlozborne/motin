with ada.exceptions; use ada.exceptions;
with Ada.Characters.Handling; use ada.characters.handling;
with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;

with api39dll; use api39dll;

package body TcpIp is

   function sendTCPMessage(msg : msgType) return integer is
   begin
      Cvalue := ClearBuffer(cZero);
      for i in 1..msg.size loop
         cvalue := writeByte(c.double(msg.byteArray(i)), cZero);
      end loop;
      return integer(SendMessage(ConnectSocket, CEmptyString, CZero, CZero));
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.sendTCPMessage --" & kLFString & Exception_Information (error));
         raise;
   end sendTCPMessage;

   function receiveTCPMessageBlocking return msgType is   
      msg     : msgType;
   begin
      Cvalue := ReceiveMessage(              -- blocking
         Sockid => ConnectSocket,
         Len    => C.Double(0),
         Buffid => C.Double(0));
      msg.size := integer(cvalue);

      -- if size > kMaxLenMsg then size := kMaxLenMsg; end if;

      for i in 1..msg.size loop
         msg.byteArray(i) := unsigned_8(readByte(cZero));
      end loop;
      return msg;

   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in MessageTranslationLibrary.receiveTCPMessageBlocking --" & kLFString & Exception_Information (error));
         raise;
   end receiveTCPMessageBlocking;
   
   function makeTcpConnection return boolean is
      Connected          : Boolean := False;
      line               : string(1..100);
      lineLen            : natural;
      num                : natural;
      pos                : natural;
      char               : character := 'S';
      haveInputs         : boolean;
      inControllerMode   : boolean := true;
   begin
      -- loop
         -- put("Enter Standalone mode or Controller mode (S/C): ");
         -- get(char);
         -- skip_Line;
         -- char := to_upper(char);
         -- exit when char = 'S' or char = 'C';
      -- end loop;
      -- inControllerMode := (char = 'C');
   
      IpStrC := New_String("127.0.0.1");

      inControllerMode := true;
      new_line;
      Put_Line("Trying to connect to controller using IP 127.0.0.1 and port 1235,");
      ServerPort := c.double(1235); 
      
      -- inControllerMode := false;   
      -- Put_Line("Trying to connect to simulator using IP 127.0.0.1 and port 1234,");
      -- ServerPort := c.double(1234);
         
      -- inControllerMode := false;     
      -- Put_Line("Using IP 127.0.0.1 and port 1236,");
      -- ServerPort := c.double(1236);

      put_line("and if that doesn't work, you will get a chance to change the values.");
      haveInputs := true;
      CValue := DllInit;
      LOOP
      
         if haveInputs then
            new_line;
            put_line("Tryng to connect ...............");
            ConnectSocket := TcpConnect(
               Ip   => IpStrC,
               Port => ServerPort,
               Mode => BlockingMode);
         end if;
            
      EXIT WHEN Integer(ConnectSocket) > 0;
         
         new_line;
         put_line("ERROR: bad ip address or port number --");

         begin
            new_line;
            put("Enter IP address of server or 0 to keep default: ");
            get_line(line, lineLen);
            get(line(1..lineLen), num, pos);
            if num /= 0 then
              IpStrC := New_String(line(1..lineLen));
            end if;

            put("Enter port number of server or 0 to keep default: ");
            get_line(line, lineLen);
            get(line(1..lineLen), num, pos);
            if num /= 0 then
              ServerPort := c.double(num);
            end if;
            haveInputs := true;
         exception
            when error : others =>
               haveInputs := false;
          end;

      END LOOP;

      new_line; put_line("Connected"); new_line;
      return inControllerMode;
   end makeTcpConnection;
   
end TcpIp;