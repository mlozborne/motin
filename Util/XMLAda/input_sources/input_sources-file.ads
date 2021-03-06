-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2008, AdaCore            --
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

with Unicode;
with Unicode.CES;

package Input_Sources.File is

   type File_Input is new Input_Source with private;
   type File_Input_Access is access all File_Input'Class;
   --  A special implementation of a reader, that reads from a file.

   procedure Open (Filename : String; Input : out File_Input'Class);
   --  Open a new file for reading.
   --  Note that the file is read completly at once, and saved in memory.
   --  This provides a much better access later on, however this might be a
   --  problem for very big files.
   --  The physical file on disk can be modified at any time afterwards, since
   --  it is no longer read.
   --  This function can decode a file if it is coded in Utf8, Utf16 or Utf32
   --  This function must raise Name_Error if the file does not exist

   procedure Close (Input : in out File_Input);
   --  Close the file and free the memory

   procedure Next_Char
     (From : in out File_Input;
      C    : out Unicode.Unicode_Char);
   --  Return the next character in the file.

   function Eof (From : File_Input) return Boolean;
   --  True if From is past the last character in the file.

   procedure Set_System_Id
     (Input : in out File_Input; Id : Unicode.CES.Byte_Sequence);
   --  Override Input_Sources.Set_System_Id, and ensure we use an absolute
   --  file name. This is needed in lots of cases, for instance to resolve
   --  relative URIs, to ensure we do not parse a grammar twice,...

private
   type File_Input is new Input_Source with
      record
         Index  : Natural;
         Buffer : Unicode.CES.Byte_Sequence_Access;
      end record;
end Input_Sources.File;
