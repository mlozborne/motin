WITH Interfaces.C.Strings; USE Interfaces.C.Strings;
USE Interfaces;
with messageTranslationTypes; 

package TcpIp is

   subtype msgType is messageTranslationTypes.messageType;

   function sendTcpMessage(msg : msgType) return integer;
   function receiveTcpMessageBlocking return msgType;   
   procedure makeTcpConnection(IP : string; Port : string);   
   
private
   kLFString           : string(1..1) := ( 1=> standard.ascii.LF);   
   ConnectSocket       : C.Double;
   IpStrC              : Chars_Ptr;
   CEmptyString        : Chars_Ptr := New_String("");
   ServerPort          : c.double;
   Buffer0             : C.Double := C.Double(0.0);
   BlockingMode        : C.double := c.double(0);
   NonblockingMode     : C.double := c.double(1);
   CValue              : C.Double;
   CZero               : C.double := c.double(0); 
end TcpIp;