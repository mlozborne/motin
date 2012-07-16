WITH Unicode.CES, LayoutPkg, Ada.Text_IO, Ada.Exceptions, ControllerGlobals;
USE Unicode.CES, LayoutPkg, Ada.Text_IO, Ada.Exceptions, ControllerGlobals;
with MessageTranslationTypes; use messageTranslationTypes;


PACKAGE BODY XMLParser IS

--vvvvvvvvvvvvvvvvvvvvvvvv Helper functions vvvvvvvvvvvvvvvvvvvvvvvvv

   ----------------------------
   ---------- Section ---------
   ----------------------------

   PROCEDURE Get_Section_Values (
         Atts : Sax.Attributes.Attributes'Class) IS
      Id : Positive;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         Id := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      LayoutPtr.NewSection(Id);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("*********** EXCEPTION Error adding section:" & Positive'Image(Id) & " " & Exception_Information(Error));
         RAISE;
   END Get_Section_Values;


   ----------------------------
   ---------- Sensor ----------
   ----------------------------

   PROCEDURE Get_Sensor_Values (
         Atts : Sax.Attributes.Attributes'Class) IS
      Id : Positive;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         Id := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      LayoutPtr.AddSensor(Id);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("************ EXCEPTION Error adding sensor:" & Positive'Image(Id) & " " & Exception_Information(Error));
         RAISE;
   END Get_Sensor_Values;


   ----------------------------
   ---------- Switch ----------
   ----------------------------

   PROCEDURE Get_Switch_Values (
         Atts : Sax.Attributes.Attributes'Class) IS
      Id    : Positive;
      -- State : SwitchStateType;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         Id := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      -- IF Get_Index(Atts, "state") /= -1 THEN
         -- State := SwitchStateType'Value(Get_Value(Atts, "state"));
      -- END IF;
      LayoutPtr.AddSwitch(Id);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("************* EXCEPTION Error adding switch:" & Positive'Image(Id) & " " & Exception_Information(Error));
         RAISE;
   END Get_Switch_Values;

   PROCEDURE Update_Switch (
         Atts : Sax.Attributes.Attributes'Class) IS
      Id           : Positive;
      TypeOfSwitch : ControllerGlobals.SwitchType;
      state        : switchStateType;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         Id := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      IF Get_Index(Atts, "state") /= -1 THEN
         State := SwitchStateType'Value(Get_Value(Atts, "state"));
      END IF;
      IF Get_Index(Atts, "type") /= -1 THEN
         TypeOfSwitch := ControllerGlobals.SwitchType'Value(Get_Value(Atts, "type"));
      ELSE
         TypeOfSwitch := ControllerGlobals.SwitchType'Value("Normal");
      END IF;
      LayoutPtr.UpdateSwitch(Id, TypeOfSwitch, state);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("************* EXCEPTION Error updating switch:" & Positive'Image(Id) & " " & Exception_Information(Error));
         RAISE;
   END Update_Switch;

   PROCEDURE Update_Switch_Narrow (
         Atts : Sax.Attributes.Attributes'Class) IS
      NarrowId : Positive;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         NarrowId := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      LayoutPtr.UpdateSwitchNarrow(NarrowId);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("*************** EXCEPTION Error updating switch narrow end. Sensor Id:" & Positive'Image(NarrowId) & " " & Exception_Information(Error));
         RAISE;
   END Update_Switch_Narrow;

   PROCEDURE Update_Switch_Closed (
         Atts : Sax.Attributes.Attributes'Class) IS
      ClosedId : Positive;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         ClosedId := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      LayoutPtr.UpdateSwitchClosed(ClosedId);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("*********** EXCEPTION Error updating switch closed end. Sensor Id:" & Positive'Image(ClosedId) & " " & Exception_Information(Error));
         RAISE;
   END Update_Switch_Closed;

   PROCEDURE Update_Switch_Thrown (
         Atts : Sax.Attributes.Attributes'Class) IS
      ThrownId : Positive;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         ThrownId := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      LayoutPtr.UpdateSwitchThrown(ThrownId);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("*************** EXCEPTION Error updating switch thrown end. Sensor Id:" & Positive'Image(ThrownId) & " " & Exception_Information(Error));
         RAISE;
   END Update_Switch_Thrown;


   ---------------------------------
   ---------- Blocking -------------
   ---------------------------------

   PROCEDURE Get_Blocking_Values (
         Atts : Sax.Attributes.Attributes'Class) IS
      Id : Positive;
   BEGIN
      IF Get_Index(Atts, "id") /= -1 THEN
         Id := Positive'Value(Get_Value(Atts, "id"));
      END IF;
      LayoutPtr.AddBlocking(Id);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("************** EXCEPTION Error getting blocking values " & Exception_Information(Error));
         RAISE;
   END Get_Blocking_Values;

--^^^^^^^^^^^^^^^^^^^^^^^^^ Helper functions ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   --------------------------------
   ------ XML/Ada Procedures ------
   --------------------------------

   PROCEDURE Start_Element (
         Handler       : IN OUT Reader;
         Namespace_URI :        Unicode.CES.Byte_Sequence       := "";
         Local_Name    :        Unicode.CES.Byte_Sequence       := "";
         Qname         :        Unicode.CES.Byte_Sequence       := "";
         Atts          :        Sax.Attributes.Attributes'Class) IS
   BEGIN
      IF Local_Name = "section-list" THEN
         SectionList := True;
         SwitchList := False;
      END IF;
      IF Local_Name = "switch-list" THEN
         SwitchList := True;
         SectionList := False;
      END IF;
      IF SectionList THEN
         IF Local_Name = "section" THEN
            Get_Section_Values(Atts);
         ELSIF Local_Name = "sensor" THEN
            Get_Sensor_Values(Atts);
         ELSIF Local_Name = "switch" THEN
            Get_Switch_Values(Atts);
         ELSIF Local_Name = "blocking" THEN
            Get_Blocking_Values(Atts);
         END IF;
      ELSIF SwitchList THEN
         IF Local_Name = "switch" THEN
            Update_Switch(Atts);
         ELSIF Local_Name = "narrow-end" THEN
            Update_Switch_Narrow(Atts);
         ELSIF Local_Name = "closed-end" THEN
            Update_Switch_Closed(Atts);
         ELSIF Local_Name = "thrown-end" THEN
            Update_Switch_Thrown(Atts);
         END IF;
      END IF;
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("*********** EXCEPTION Error reading XML starting tag " & Exception_Information(Error));
         RAISE;
   END Start_Element;

   PROCEDURE Characters (
         Handler : IN OUT Reader;
         Ch      :        Unicode.CES.Byte_Sequence) IS
   BEGIN
      NULL;
   END Characters;

   PROCEDURE End_Element (
         Handler       : IN OUT Reader;
         Namespace_URI :        Unicode.CES.Byte_Sequence := "";
         Local_Name    :        Unicode.CES.Byte_Sequence := "";
         Qname         :        Unicode.CES.Byte_Sequence := "") IS
   BEGIN
      IF Local_Name = "section" THEN
         LayoutPtr.EndSection;
      ELSIF Local_Name = "section-list" THEN
         LayoutPtr.EndSectionList;
      ELSIF Local_Name = "switch-list" THEN
         LayoutPtr.EndSwitchList;
      END IF;
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("*********** EXCEPTION Error reading XML ending tag " & Exception_Information(Error));
         RAISE;
   END End_Element;

   PROCEDURE SetLayout (
         L : LayoutManagerAccess) IS
   BEGIN
      LayoutPtr := L;
   END SetLayout;

END XMLParser;