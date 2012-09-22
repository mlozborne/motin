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
		

   procedure establishListenerSocket(
				 socket     : out socketType;
	          Port       : unbounded_String;
				 maxClients : natural;
				 blocking   : boolean) is
		blockingMode : c.double;
		cvalue		 : c.double;
   begin
      
		Put_Line("Trying to establish a listener socket for port " & to_string(Port)); 
					
		socket.blocking := blocking;
		socket.port := port;
		socket.ip := to_unbounded_string("listener");
		
		if blocking then
			blockingMode := c.double(0);
		else
			blockingMode := c.double(1);
		end if;
		
		socket.number := TcpListen(
			Port => c.double(integer'value(to_string(port))),
			Max => c.double(maxClients),
			Mode => BlockingMode);  
			
		if integer(socket.number) > 0 then
			socket.connected := true;
		end if;
		
		Put_Line("Finished trying to establish a listener socket for port " & to_string(Port));
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.establishListenerSocket --" & kLFString & Exception_Information (error));
         raise;
   end establishListenerSocket;
   
   procedure acceptClient(
				 listenSocket   : socketType;     
				 socketToClient : out socketType; 
				 timeDelay      : duration;
			 	 blocking       : boolean);
		blockingMode : c.double;
		cvalue		 : c.double;
   begin
      loop
         Put_Line("Trying to connect to IP " & to_string(IP) & " and port " & to_string(Port)); 
						
		   socket.IP := IP;
			socket.blocking := blocking;
			socket.port := port;
			
			if blocking then
				blockingMode := c.double(0);
			else
				blockingMode := c.double(1);
			end if;
			
         socket.number := TcpConnect(
            Ip   => New_String(to_string(IP)),
            Port => c.double(integer'value(to_string(port))),
            Mode => BlockingMode);  
				
         exit when integer(socket.number) > 0;
			
			if timeDelay > 0.0 then
				delay timeDelay;
			end if;
      end loop;
		socket.connected := true;
		
		Put_Line("Connected to IP " & to_string(IP) & " and port " & to_string(Port));
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
		cvalue		 : c.double;
   begin
      loop
         Put_Line("Trying to connect to IP " & to_string(IP) & " and port " & to_string(Port)); 
						
		   socket.IP := IP;
			socket.blocking := blocking;
			socket.port := port;
			
			if blocking then
				blockingMode := c.double(0);
			else
				blockingMode := c.double(1);
			end if;
			
         socket.number := TcpConnect(
            Ip   => New_String(to_string(IP)),
            Port => c.double(integer'value(to_string(port))),
            Mode => BlockingMode);  
				
         exit when integer(socket.number) > 0;
			
			if timeDelay > 0.0 then
				delay timeDelay;
			end if;
      end loop;
		socket.connected := true;
		
		Put_Line("Connected to IP " & to_string(IP) & " and port " & to_string(Port));
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
		if not socket.connected then
			size := -9999;
			raise socketNotConnected;
		end if;
      Cvalue := ClearBuffer(cZero);
      for i in 1..msg.size loop
         cvalue := writeByte(c.double(msg.byteArray(i)), cZero);
      end loop;
      size := integer(SendMessage(socket.number, CEmptyString, CZero, CZero));
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
		if not socket.connected then
			size := -9999;
			msg.size := 0;
			raise socketNotConnected;
		end if;
      Cvalue := ReceiveMessage(              
         Sockid => socket.number,
         Len    => CZero,
         Buffid => CZero);
      msg.size := integer(cvalue);
		size := integer(cvalue);
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
		return ("IP " & to_string(socket.ip) 
		        & " port " & to_string(socket.port)
				  & " connected " & boolean'image(socket.connected)
				  & " blocking " & boolean'image(socket.blocking)
				  & " socket number " & integer'image(integer(socket.number)));
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in TcpIp.toSting --" & kLFString & Exception_Information (error));
         raise;
   end toString;
   
end TcpIp;