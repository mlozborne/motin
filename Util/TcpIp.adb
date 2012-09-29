with ada.exceptions; use ada.exceptions;
with Ada.Characters.Handling; use ada.characters.handling;
with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;

with api39dll; use api39dll;

package body TcpIp is
   
	procedure initialize39DLL is 
		cvalue		 : c.double;
	begin	
		cvalue := DllInit;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.initialize39DLL --" & kLFString & Exception_Information (error));
         raise;
   end initialize39DLL;
		
	function zeroSocket return socketType is
	begin	
		return c.double(0);
	end;
	
   procedure establishListenerSocket(
				 socket     : out socketType;
	          Port       : unbounded_String;
				 maxClients : natural;
				 blocking   : boolean) is
		blockingMode : c.double;
   begin
      
		Put_Line("    In EstablishListenerSocket for port " & to_string(Port)); 
							
		if blocking then
			blockingMode := c.double(0);
		else
			blockingMode := c.double(1);
		end if;
		
		socket := TcpListen(
			Port => c.double(integer'value(to_string(port))),
			Max => c.double(maxClients),
			Mode => BlockingMode);       -- this determines the mode of the listener socket 

      Put_Line("    In EstablishListenerSocket established socket " & integer'image(integer(socket))); 			
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.establishListenerSocket --" & kLFString & Exception_Information (error));
         raise;
   end establishListenerSocket;
   
   procedure acceptClient(
				 listenSocket   : socketType;     
				 socketToClient : out socketType; 
				 timeDelay      : duration;
			 	 blocking       : boolean) is
		blockingMode : c.double;
   begin
      Put_Line("    In AcceptClient using listen socket " & integer'image(integer(listenSocket))); 
      loop
						
			if blocking then
				blockingMode := c.double(0);
			else
				blockingMode := c.double(1);
			end if;
			
         socketToClient := TcpAccept(
				sockId => listenSocket,
            Mode => BlockingMode);    -- this determines the mode of send/receive on this socket
				
         exit when integer(socketToClient) > 0;
			
			if timeDelay > 0.0 then
				delay timeDelay;
			end if;
      end loop;		
      Put_Line("    In AcceptClient have established socket to client with " & integer'image(integer(socketToClient))); 
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.acceptClient --" & kLFString & Exception_Information (error));
         raise;
   end acceptClient;

   procedure connectToServer(
				 socket     : out socketType;
	          IP         : unbounded_String;  
	          Port       : unbounded_String;
				 timeDelay  : duration; 
				 blocking   : boolean) is
		blockingMode : c.double;
   begin
      loop
         Put_Line("    In ConnectToServer using IP " & to_string(IP) & " and port " & to_string(Port)); 
									
			if blocking then
				blockingMode := c.double(0);
			else
				blockingMode := c.double(1);
			end if;
			
         socket := TcpConnect(
            Ip   => New_String(to_string(IP)),
            Port => c.double(integer'value(to_string(port))),
            Mode => BlockingMode);   -- this determines the mode of send/receive on this socket
				
         exit when integer(socket) > 0;
			
			if timeDelay > 0.0 then
				delay timeDelay;
			end if;
      end loop;
		
		Put_Line("    In ConnectToServer now connected to IP " & to_string(IP) & 
					" and port " & to_string(Port) & 
					" with socket " & integer'image(integer(socket)));
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.connectToServer --" & kLFString & Exception_Information (error));
         raise;
   end connectToServer;

   procedure sendMessage(
	          socket : socketType; 
	          msg    : msgType; 
				 size   : out integer) is   
      CZero        : C.double := c.double(0); 
		cvalue		 : c.double;
      CEmptyString : Chars_Ptr := New_String("");
   begin
		if integer(socket) <= 0 then
			raise socketNotEstablished;
		end if;
      Cvalue := ClearBuffer(cZero);
      for i in 1..msg.size loop
         cvalue := writeByte(c.double(msg.byteArray(i)), cZero);
      end loop;
      size := integer(SendMessage(socket, CEmptyString, CZero, CZero));
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.sendMessage --" & kLFString & Exception_Information (error));
         raise;
   end sendMessage;

   procedure receiveMessage(
	          socket : socketType;
				 msg    : out msgType;
				 size   : out integer) is
      CZero        : C.double := c.double(0); 
	   cvalue		 : c.double;
   begin
		if integer(socket) <= 0 then
			raise socketNotEstablished;
		end if;
      Cvalue := ReceiveMessage(              
         Sockid => socket,
         Len    => CZero,
         Buffid => CZero);
		size := integer(cvalue);
		
		if size <= 0 then
			msg.size := 0;
			return;
		end if;
		
      msg.size := size;
      for i in 1..msg.size loop
         msg.byteArray(i) := unsigned_8(readByte(cZero));
      end loop;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.receiveMessage --" & kLFString & Exception_Information (error));
         raise;
   end receiveMessage;

	function toString(socket : socketType) return string is
	begin	
		return (integer'image(integer(socket)));
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.toSting --" & kLFString & Exception_Information (error));
         raise;
   end toString;
   
end TcpIp;