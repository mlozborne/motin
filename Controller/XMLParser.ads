WITH Sax.Readers, Unicode.CES, Sax.Attributes, LayoutPkg;
USE Sax.Readers, Unicode.CES, Sax.Attributes, LayoutPkg;

PACKAGE XMLParser IS
   TYPE String_Access IS ACCESS String;

   TYPE Reader IS NEW Sax.Readers.Reader WITH NULL RECORD;

   PROCEDURE Start_Element (
         Handler       : IN OUT Reader;
         Namespace_URI :        Unicode.CES.Byte_Sequence       := "";
         Local_Name    :        Unicode.CES.Byte_Sequence       := "";
         Qname         :        Unicode.CES.Byte_Sequence       := "";
         Atts          :        Sax.Attributes.Attributes'Class);

   PROCEDURE End_Element (
         Handler       : IN OUT Reader;
         Namespace_URI :        Unicode.CES.Byte_Sequence := "";
         Local_Name    :        Unicode.CES.Byte_Sequence := "";
         Qname         :        Unicode.CES.Byte_Sequence := "");

   PROCEDURE Characters (
         Handler : IN OUT Reader;
         Ch      :        Unicode.CES.Byte_Sequence);

   PROCEDURE SetLayout (
         L : LayoutPkg.LayoutManagerAccess);

PRIVATE
   LayoutPtr : LayoutPkg.LayoutManagerAccess;
   SectionList : Boolean := False;
   SwitchList : Boolean := False;
END XMLParser;