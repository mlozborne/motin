-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2004                     --
--                            ACT-Europe                             --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-----------------------------------------------------------------------

--  This package provides support for the ISO/8859-15 (aka Latin-15)
--  encoding.
--  This is a superset of ISO/8859-1, but also includes the Euro symbol

package Unicode.CCS.Iso_8859_15 is

   Name1 : aliased constant String := "ISO-8859-15";

   function To_Unicode     (Char : Unicode_Char) return Unicode_Char;
   function To_Iso_8859_15 (Char : Unicode_Char) return Unicode_Char;

   Iso_8859_15_Character_Set : constant Character_Set :=
     (To_Unicode => To_Unicode'Access,
      To_Cs      => To_Iso_8859_15'Access);

end Unicode.CCS.Iso_8859_15;
