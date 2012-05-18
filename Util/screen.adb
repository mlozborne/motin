PRAGMA C_Pass_By_Copy (128);  -- For interfacing with Windows

WITH Interfaces.C;
with ada.text_io; use ada.text_io;
WITH Ada.Exceptions; USE Ada.Exceptions;

PACKAGE BODY Screen IS

  ------------------------------------------------------------------
  --|
  --| body of screen-handling package
  --|
  --| Author: Michael B. Feldman, The George Washington University
  --| Last Modified: July 1995
  --|
  ------------------------------------------------------------------
  --|
  --| Adapted to Win32 by Jerry van Dijk, june 1998
  --|
  ------------------------------------------------------------------

  -----------------------
  -- Win32 Interfacing --
  -----------------------

  TYPE Win32_Bool IS NEW Interfaces.C.Int;
  TYPE Win32_Tchar IS NEW Interfaces.C.Char;
  TYPE Win32_Short IS NEW Interfaces.Unsigned_16;
  TYPE Win32_Dword IS NEW Interfaces.Unsigned_32;
  TYPE Win32_Handle IS NEW Interfaces.Unsigned_32;

  TYPE Win32_Lpdword IS ACCESS ALL Win32_Dword;
  PRAGMA Convention (C, Win32_Lpdword);

  TYPE Win32_Coord IS
  RECORD
    X : Win32_Short;
    Y : Win32_Short;
  END RECORD;
  PRAGMA Convention (C, Win32_Coord);

  FUNCTION Win32_Beep (Frequency, Duration : Win32_Dword) RETURN Win32_Bool;
  PRAGMA Import (Stdcall, Win32_Beep, "Beep");

  FUNCTION Win32_Getstdhandle (Nstdhandle : Win32_Dword) RETURN Win32_Handle;
  PRAGMA Import (Stdcall, Win32_Getstdhandle, "GetStdHandle");

  FUNCTION Win32_Setconsolecursorposition
    (Console : Win32_Handle;
    Pos : Win32_Coord)
    RETURN Win32_Bool;
  PRAGMA Import (Stdcall, Win32_Setconsolecursorposition, "SetConsoleCursorPosition");

  FUNCTION Win32_Fillconsoleoutputcharacter
    (Console : Win32_Handle;
    Char    : Win32_Tchar;
    Length  : Win32_Dword;
    Start   : Win32_Coord;
    Written : Win32_Lpdword)
    RETURN Win32_Bool;
  PRAGMA Import (Stdcall, Win32_Fillconsoleoutputcharacter, "FillConsoleOutputCharacterA");

  Win32_Beep_Error           : EXCEPTION;
  Win32_Cursor_Pos_Error     : EXCEPTION;
  Win32_Fill_Screen_Error    : EXCEPTION;
  Win32_Invalid_Handle_Error : EXCEPTION;

  Win32_False                : CONSTANT Win32_Bool   := 0;
  Win32_Screen_Size          : CONSTANT Win32_Dword  := 2000;
  Win32_Invalid_Handle_Value : CONSTANT Win32_Handle := -1;
  Win32_Std_Output_Handle    : CONSTANT Win32_Dword  := -11;

  Win32_Output_Buffer : Win32_Handle;
  Num_Bytes           : ALIASED Win32_Dword;
  Num_Bytes_Access    : Win32_Lpdword := Num_Bytes'ACCESS;

  ----------------------------
  -- Package Implementation --
  ----------------------------

  PROCEDURE Beep IS
    Result : Win32_Bool;
  BEGIN
    Result := Win32_Beep (Frequency => 1000, Duration => 750);
    IF Result = Win32_False THEN
      RAISE Win32_Beep_Error;
    END IF;
  EXCEPTION
     WHEN Error: OTHERS =>
        Put_Line("UNPLANNED EXCEPTION in Screen.Beep --" & LFString & Exception_Information (Error));
        raise;
  END Beep;

  PROCEDURE Clearscreen IS
    Result : Win32_Bool;
    Home   : Win32_Coord := (0, 0);
  BEGIN
   Result := Win32_Fillconsoleoutputcharacter
      (Console => Win32_Output_Buffer,
      Char     => ' ',
      Length   => Win32_Screen_Size,
      Start    => Home,
      Written  => Num_Bytes_Access);
   IF Result = Win32_False THEN
      RAISE Win32_Fill_Screen_Error;
   END IF;
   Result := Win32_Setconsolecursorposition (Console => Win32_Output_Buffer, Pos => Home);
   IF Result = Win32_False THEN
      RAISE Win32_Cursor_Pos_Error;
   END IF;
  EXCEPTION
     WHEN Error: OTHERS =>
        Put_Line("UNPLANNED EXCEPTION in Screen.ClearScreen --" & LFString & Exception_Information (Error));
        raise;
  END Clearscreen;

  PROCEDURE Movecursor (Column : Width; Row : Depth) IS
    Result  : Win32_Bool;
    New_Pos : Win32_Coord;
  BEGIN
    New_Pos.Y := Win32_Short (Row) - 1;
    New_Pos.X := Win32_Short (Column) - 1;
    Result := Win32_Setconsolecursorposition (Console => Win32_Output_Buffer, Pos => New_Pos);
    IF Result = Win32_False THEN
      RAISE Win32_Cursor_Pos_Error;
    END IF;
  EXCEPTION
     WHEN Error: OTHERS =>
        Put_Line("UNPLANNED EXCEPTION in Screen.MoveCursor --" & LFString & Exception_Information (Error));
        raise;
  END Movecursor;

  --------------------------
  -- Win32 Initialization --
  --------------------------

BEGIN
  Win32_Output_Buffer := Win32_Getstdhandle (Win32_Std_Output_Handle);
  IF Win32_Output_Buffer = Win32_Invalid_Handle_Value THEN
    RAISE Win32_Invalid_Handle_Error;
  END IF;
END Screen;