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

      for i in 1..msg.size loop
         msg.byteArray(i) := unsigned_8(readByte(cZero));
      end loop;
      return msg;

   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.receiveTCPMessageBlocking --" & kLFString & Exception_Information (error));
         raise;
   end receiveTCPMessageBlocking;
   
   procedure makeTcpConnection(IP : string; Port : string) is
      Connected          : Boolean := False;
   begin
      loop
         Put_Line("Trying to connect to controller using IP " & IP & " and port " & Port);
         IpStrC := New_String(IP);
         CValue := DllInit;
         ServerPort := c.double(integer'value(Port)); 
         ConnectSocket := TcpConnect(
            Ip   => IpStrC,
            Port => ServerPort,
            Mode => BlockingMode);     
         -- if integer(connectSocket) <= 0 then
            -- raise ConnectionError;
         -- end if;
         exit when integer(connectSocket) > 0;
      end loop;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.makeTcpConnection --" & kLFString & Exception_Information (error));
         raise;
   end makeTcpConnection;
   
end TcpIp;