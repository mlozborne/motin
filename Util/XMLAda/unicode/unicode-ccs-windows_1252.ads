-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2005                          --
--                            AdaCore                                --
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

--  This package provides support for the Windows 1252 encoding

package Unicode.CCS.Windows_1252 is

   Name1 : aliased constant String := "Windows-1252";

   function To_Unicode      (Char : Unicode_Char) return Unicode_Char;
   function To_Windows_1252 (Char : Unicode_Char) return Unicode_Char;

   Windows_1252_Character_Set : constant Character_Set :=
     (To_Unicode => To_Unicode'Access,
      To_Cs      => To_Windows_1252'Access);
end Unicode.CCS.Windows_1252;
