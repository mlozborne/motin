-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                Copyright (C) 2001-2008, AdaCore                   --
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

--  This is the root of the hierarchy that provides coded character sets.
--  These CCS can be used to convert from any encoding (like Iso/Latin-1)
--  to Unicode.
--  See http://www.isoc.org:8080/codage/iso8859/jeuxiso.en.htm for a list
--  of the ISO 8859-*  character sets
--  ??? URL above is invalid

package Unicode.CCS is

   --  Each of the child package shall have two public functions with
   --  the following profile:
   --    function Convert (Char : Unicode_Char) return Unicode_Char
   --  that converts from a code point representing an abstract character in
   --  the specific encoding to the code point for the same character in
   --  Unicode, or reverse.

   --  ??? This is not consistent with the specification of type Unicode_Char,
   --  which states that values of Unicode_Char always are code points in
   --  the Unicode repertoire.

   type Conversion_Function is access
     function (Char : Unicode_Char) return Unicode_Char;

   type Character_Set is record
      To_Unicode : Conversion_Function;
      To_CS      : Conversion_Function;
   end record;

   --------------------
   -- Character sets --
   --------------------

   function Identity (Char : Unicode_Char) return Unicode_Char;
   --  return its parameter directly.

   Unicode_Character_Set : constant Character_Set :=
     (To_Unicode => Identity'Access,
      To_Cs      => Identity'Access);

   Invalid_Code : exception;
   --  Exception raised when trying to convert to a code point that
   --  is not available in the specific character set. This can never be
   --  raised when converting to Unicode, since it is universal by definition.

   Unknown_Character_Set : exception;
   --  Raised by Get_Conversion or Get_Revert_Conversion when the name is
   --  unknown.

end Unicode.CCS;
