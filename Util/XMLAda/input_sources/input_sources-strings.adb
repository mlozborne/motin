-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2002                     --
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

with Unicode;      use Unicode;
with Unicode.CES;  use Unicode.CES;
with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CES.Utf16;  use Unicode.CES.Utf16;
with Unicode.CES.Utf8;   use Unicode.CES.Utf8;

package body Input_Sources.Strings is

   ----------
   -- Open --
   ----------

   procedure Open
     (Str      : Unicode.CES.Byte_Sequence_Access;
      Encoding : Unicode.CES.Encoding_Scheme;
      Input    : out String_Input)
   is
      BOM : Bom_Type;
   begin
      Input.Encoding := Encoding;
      Input.Buffer := Str;
      Input.Index := Input.Buffer'First;
      Input.Free_Buffer := False;

      Read_Bom (Input.Buffer.all, Input.Prolog_Size, BOM);
      case BOM is
         when Utf32_LE =>
            Set_Encoding (Input, Utf32_LE_Encoding);
         when Utf32_BE =>
            Set_Encoding (Input, Utf32_BE_Encoding);
         when Utf16_LE =>
            Set_Encoding (Input, Utf16_LE_Encoding);
         when Utf16_BE =>
            Set_Encoding (Input, Utf16_BE_Encoding);
         when Ucs4_BE | Ucs4_LE | Ucs4_2143 | Ucs4_3412 =>
            raise Invalid_Encoding;
         when Utf8_All | Unknown =>
            Set_Encoding (Input, Utf8_Encoding);
      end case;
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Str      : Unicode.CES.Byte_Sequence;
      Encoding : Unicode.CES.Encoding_Scheme;
      Input    : out String_Input)
   is
      BOM : Bom_Type;
   begin
      Input.Encoding := Encoding;
      Input.Buffer := new Byte_Sequence'(Str);
      Input.Index := Input.Buffer'First;
      Input.Free_Buffer := True;

      Read_Bom (Input.Buffer.all, Input.Prolog_Size, BOM);
      case BOM is
         when Utf32_LE =>
            Set_Encoding (Input, Utf32_LE_Encoding);
         when Utf32_BE =>
            Set_Encoding (Input, Utf32_BE_Encoding);
         when Utf16_LE =>
            Set_Encoding (Input, Utf16_LE_Encoding);
         when Utf16_BE =>
            Set_Encoding (Input, Utf16_BE_Encoding);
         when Ucs4_BE | Ucs4_LE | Ucs4_2143 | Ucs4_3412 =>
            raise Invalid_Encoding;
         when Utf8_All | Unknown =>
            Set_Encoding (Input, Utf8_Encoding);
      end case;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out String_Input) is
   begin
      if Input.Free_Buffer then
         Free (Input.Buffer);
      end if;
      Input_Sources.Close (Input_Source (Input));
   end Close;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out String_Input;
      C    : out Unicode.Unicode_Char) is
   begin
      From.Encoding.Read (From.Buffer.all, From.Index, C);
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : String_Input) return Boolean is
   begin
      return From.Index > From.Buffer'Last;
   end Eof;

end Input_Sources.Strings;
