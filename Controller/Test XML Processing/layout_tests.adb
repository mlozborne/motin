--  WITH AUnit.Assertions;
--  USE AUnit.Assertions;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with LayoutPkg;

package body Layout_Tests is

   procedure Run_Test (
         T : in out Test) is
      --  PRAGMA Unreferenced(T);
      LayoutPtr : constant LayoutPkg.LayoutManagerAccess
         := new LayoutPkg.LayoutManager;
   begin
      LayoutPtr.SetXMLFilename (To_Unbounded_String ("newXMLLayout.xml"));
      Assert (T, LayoutPkg.ParseXML (LayoutPtr), "something");
   end Run_Test;

   function Name (
         T : Test)
         return AUnit.Message_String is
            pragma Unreferenced (T);
   begin
      return AUnit.Format ("Layout tests");
   end Name;

end Layout_Tests;
