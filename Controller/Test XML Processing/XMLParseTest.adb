WITH LayoutPkg, Ada.Strings.Unbounded, Ada.Text_IO, Globals, CommandQueueManager, Interfaces, Ada.Exceptions;
USE LayoutPkg, Ada.Strings.Unbounded, Ada.Text_IO, Globals, CommandQueueManager, Interfaces, Ada.Exceptions;

PROCEDURE XMLParseTest IS
   LayoutPtr : LayoutPkg.LayoutManagerAccess :=
   NEW LayoutPkg.LayoutManager;
   Output      : File_Type;
   XMLFilename : String      := "xxx";
   Result      : Boolean;
BEGIN
   LayoutPtr.SetXMLFilename(To_Unbounded_String(XMLFilename));
   Result := ParseXML(LayoutPtr);
   IF Result THEN
      Put_Line("parsed");
      Create (Output, Out_File, "out.txt");
      Put_Line(Output, "Section List:");
      LayoutPtr.Print_Sections(LayoutPtr.GetSectionList, 0, Output);
      Put_Line(Output, "");
      Put_Line(Output, "Sensor List:");
      LayoutPtr.Print_Sensors(LayoutPtr.GetSensorList, 0, Output);
      Put_Line(Output, "");
      Put_Line(Output, "Switch List:");
      LayoutPtr.Print_Switchs(LayoutPtr.GetSwitchList, 0, Output);
      Close(Output);
   END IF;
EXCEPTION
   WHEN Error : OTHERS =>
      Put_Line("************ EXCEPTION Error XMLParseTest:" & Exception_Information(Error));
      raise;
END XMLParseTest;
