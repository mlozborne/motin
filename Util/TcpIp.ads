WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
USE Interfaces;
with messageTranslationTypes; 

package TcpIp is

	socketNotConnected  : exception;

	type SocketType is private; 

   subtype msgType is messageTranslationTypes.messageType;

	procedure initialize39DLL;
	-- Must be done first
	
	procedure establishListenerSocket(
				 socket     : out socketType;    -- the established socket
				 port       : unbounded_String;  -- port on which server is listening for connection requests
				 maxClient  : natural;           -- max number of clients (?)
				 blocking   : boolean;           -- if true set up a socket that blocks on send and receive
	-- pre: none 
	-- post: an attempt has been made to establish a listener socket				

   procedure acceptClient(
				 listenSocket   : socketType;     -- the listener socket   
				 socketToClient : out socketType; -- the socket back to the client
				 timeDelay      : duration;       -- how long to wait between accept attempts if the listner doesn't block
			 	 blocking       : boolean);       -- if true set up a socket that blocks on send and receive
	-- pre: none 
	-- post: a socket has been establish
	-- warning: if a connection isn't established then never returns
	
   procedure connectToServer(
				 socket     : out socketType;    -- the established socket   
	          IP         : unbounded_String;  -- IP address of the server
	          Port       : unbounded_String;  -- port on which server is listening for connection requests
				 timeDelay  : duration;          -- how long to wait between connection attempts
			 	 blocking   : boolean);          -- if true set up a socket that blocks on send and receive
	-- pre: none 
	-- post: a socket has been establish
	-- warning: if a connection isn't established then never returns
	
   procedure sendMessage(
	          socket : socketType;        -- send socket 
	          msg    : msgType;           -- the message to send
				 size   : out integer);      -- the number of bytes sent or a TCP error code
	-- pre: the socket has been connected
	-- post: if not connected then raises socketNotConnected
	-- warning: if the connection is blocking then this procedure will block until message sent
	
   procedure receiveMessage(
	          socket : socketType;        -- receive socket
				 msg    : out msgType;       -- the message to receive 
				 size   : out integer);      -- the number of bytes received or a TCP error code
	-- pre: the socket has been connected
	-- post: if not connected then raises socketNotConnected
	-- warning: if the connection is blocking then this procedure will block until message received
	
	function toString(socket : socketType) return string;
	-- returns a string representation of the socket
   
private
   kLFString           : string(1..1) := ( 1=> standard.ascii.LF);   
	type SocketType is record
		IP                : unbounded_String := to_unbounded_string("unknown");
		port					: unbounded_String := to_unbounded_string("unknown");
		connected			: boolean := false;
		blocking          : boolean := false;
		number	       	: C.Double  := 0.0;
	end record;
end TcpIp;