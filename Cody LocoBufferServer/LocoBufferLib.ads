--Cody Baxter
--Fall 2010, CS 493

WITH Ada.Text_IO;
USE Ada.Text_IO;
WITH Ada.Integer_Text_IO;
USE Ada.Integer_Text_IO;
WITH Api39dll;
USE Api39dll;
WITH Interfaces.C.Strings;
USE Interfaces.C.Strings;
WITH Interfaces;
USE Interfaces;
WITH MessageTranslationLibrary;
USE MessageTranslationLibrary;

PACKAGE LocoBufferLib IS

   --StartLocoLib starts everything needed for LocoBufferLib-------
   PROCEDURE StartLocoLib;
   -------------------------------------------------------------

PRIVATE

   --Private Functions used in LocoBufferLib-----------------------
   -------------------------------------------------------------
   --This task is used to continullay accept connections on
   --the socket that was opened.
   Task Type TCPIPConnection Is
      Entry StartAccepting;
   End TCPIPConnection;

   --This function starts the com port connection to the physical layout
   --Takes in the Com Port number to attempt to connect to
   --Returns 1 if connection made, 0 if no connection made
   FUNCTION StartComConnection (ComNum : Integer) RETURN Integer;

   --Tasks used for Write from Com port and Read to Com Port----
   TASK TYPE WriteCom IS
      ENTRY StartWriteCom;
   END WriteCom;

   TASK TYPE WriteSA IS
      ENTRY StartWriteSA;
   END WriteSA;

   TASK TYPE Read IS
      ENTRY StartRead;
   END Read;
   -------------------------------------------------------------

   --Declerations for Com port communication from c file--------
   FUNCTION StartPort (Comnum : Integer) RETURN Integer;
   PRAGMA Import (C, StartPort, "StartSerialPort");

   FUNCTION ReadFromCom RETURN Interfaces.Unsigned_8;
   PRAGMA Import (C, ReadFromCom, "ReadFromSerial");

   PROCEDURE WriteToCom (Write : IN Interfaces.Unsigned_8);
   PRAGMA Import (C, WriteToCom, "WriteToSerial");
   -------------------------------------------------------------
   
   

END LocoBufferLib;
