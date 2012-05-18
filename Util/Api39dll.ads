WITH Interfaces.C.Strings;
USE Interfaces.C.Strings;
USE Interfaces;

--PACKAGE Api39Dll IS USE Interfaces;
PACKAGE Api39Dll IS

   FUNCTION dllInit RETURN C.Double;
   FUNCTION TcpListen(Port : C.Double; Max : C.Double; Mode : C.Double) RETURN C.Double;
   FUNCTION TcpConnect(Ip : C.Strings.Chars_Ptr; Port : C.Double; Mode : C.Double) RETURN C.Double;
   FUNCTION TcpAccept(Sockid : C.Double; Mode : C.Double) RETURN C.Double;

   FUNCTION ClearBuffer(Buffid : C.Double) RETURN C.Double;

   FUNCTION SendMessage(Sockid : C.Double; Ip : C.Strings.Chars_Ptr; Port : C.Double; Buffid : C.Double) RETURN C.Double;
   FUNCTION ReceiveMessage(Sockid : C.Double; Len : C.Double; Buffid : C.Double) RETURN C.Double;

   FUNCTION WriteByte(Val : C.Double; Buffid : C.Double) RETURN C.Double;
   FUNCTION WriteShort(Val : C.Double; Buffid : C.Double ) RETURN C.Double;
   FUNCTION WriteDouble(Val : C.Double; Buffid : C.Double ) RETURN C.Double;
   FUNCTION WriteString(Str: C.Strings.Chars_Ptr; Buffid : C.Double) RETURN C.Double;

   FUNCTION ReadByte(Buffid : C.Double) RETURN C.Double;
   FUNCTION ReadShort(Buffid : C.Double) RETURN C.Double;
   FUNCTION ReadDouble(Buffid : C.Double) RETURN C.Double;
   FUNCTION ReadString(Buffid : C.Double ) RETURN C.Strings.Chars_Ptr;

PRIVATE

   PRAGMA Import(C, dllInit, "dllInit");
   PRAGMA Import(C, TcpListen);
   PRAGMA Import(C, TcpConnect);
   PRAGMA Import(C, TcpAccept);

   PRAGMA Import(C, ClearBuffer);

   PRAGMA Import(C, SendMessage);
   PRAGMA Import(C, ReceiveMessage);

   PRAGMA Import(C, WriteByte);
   PRAGMA Import(C, WriteShort);
   PRAGMA Import(C, WriteDouble);
   PRAGMA Import(C, WriteString);

   PRAGMA Import(C, ReadByte);
   PRAGMA Import(C, ReadShort);
   PRAGMA Import(C, ReadDouble);
   PRAGMA Import(C, ReadString);

END Api39Dll ;