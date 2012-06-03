with Ada.Characters.Handling; use ada.characters.handling;
with ada.exceptions; use ada.exceptions;
with ada.Text_IO; use ada.Text_IO;
with ada.integer_text_io; use ada.integer_text_io;

with screen; use screen;

with NaturalListTypePkg; use NaturalListTypePkg; use NaturalListTypePkg.naturalListPkg;
with screenManager; use screenManager;
with RailroadManager; use RailroadManager;
with logFiles; use logFiles;
with messageTranslationTypes;

PACKAGE BODY KeyboardTask IS

   procedure getCommand (command            : out commandType; 
                         isInControllerMode : boolean;
                         success            : out boolean);

   TASK BODY KeyboardTaskType IS
      command            : commandType;
      success            : boolean;
      isInControllerMode : boolean := true;
   BEGIN
      ACCEPT Start(inConMode : boolean) do
         isInControllerMode := inConMode;
      end;
      delay 1.0;
      LOOP
         begin
            objscreenManager.PutPrompt(">");
            GetCommand(Command, isInControllerMode, success);
            if success then
               if command.cmd /= quit then
                  objRailroadManager.sendMessage(command);
               else
                  objScreenManager.clearTheScreen;
                  objScreenManager.tellUserCntrlC;
                  exit;
               end if;
            end if;
         end;
      END LOOP;
   exception
      when error : others =>
         put_line("UNPLANNED EXCEPTION in KeyboardTask --" & kLFString & Exception_Information (error));
   END KeyboardTaskType;

   procedure getCommand (command            : out commandType; 
                         isInControllerMode : boolean;
                         success            : out boolean) is
      nIsInRange        : boolean := true;
      badCommandLetter  : boolean := false;
      chr1, chr2        : character;
      n                 : natural;
      line              : string(1..100);
      lineLen		: natural;
      last              : natural;
      Pos               : Natural;
      timeStamp         : natural;
      i                 : natural;

      procedure GetChar(Line : in String; pos : in out natural; lineLen : in natural; chr : out character) IS
      BEGIN
         chr := ' ';                              -- get chr1
         for i in pos..lineLen loop
            chr := line(i);
            pos := i;
            exit when chr /= ' ';
         end loop;
         Chr := To_Upper(Chr);
         pos := pos + 1;
      end;

   begin
      success := false;
      command.cmd := unknown;

      Get_Line(Line, LineLen);                  -- read line of user input

      pos := 1;
      getChar(line, pos, lineLen, chr1);        -- get chr1

      IF Chr1 = '*' THEN                        -- reading commands from a script file
         get(line(pos..lineLen), timeStamp, last);
         pos := last + 1;
         if duration(timeStamp) > elapsedDuration then
            DELAY duration(timeStamp) - elapsedDuration;
         end if;
         elapsedDuration := clock - startTime;
         getChar(line, pos, lineLen, chr1);     -- get chr1 again
      end if;

      -- Echo the command to the AdminLog and the KeyboardLog
      putLineAdminLog("Keyboard: " & Line(pos-1..LineLen));
      putLineKeyboardLog(Line(pos-1..LineLen));

      -- Make sure the command is valid
      if chr1 = 'X' or chr1 = 'Z' or chr1 = 'S' or chr1 = '.' or
         chr1 = 'C' or chr1 = 'T' or chr1 = 'P' or
         chr1 = 'F' or chr1 = 'R' or chr1 = 'V' or
         chr1 = 'B' or chr1 = 'H' or chr1 = 'L' or chr1 = 'M' or chr1 = 'Q' then
         null;
      else
         objScreenManager.putError("Bad command: 1st letter invalid");
         return;
      end if;

      if isInControllerMode and chr1 = 'S' then
         objScreenManager.putError("ERROR: S is invalid in controller mode");
         return;
      elsif not isInControllerMode and (chr1 = 'X' or chr1 = 'Z') then
         objScreenManager.putError("ERROR: X and Z are invalid in standalone mode");
         return;
      end if;

      IF Chr1 = 'S' THEN                        -- if chr1 = S, then get chr2
         getChar(line, pos, lineLen, chr2);
         if chr2 = 'E' then
            command.cmd := selectLoco;
         elsif chr2 = 'T' then
            command.cmd := stealLoco;
         elsif chr2 ' 'X' then
            command.cmd := removeLoco;
         else
            objScreenManager.putError("ERROR: Bad command: 2nd letter must be E or T");
            return;
         end if;
      END IF;

      if chr1 = 'P' then                        -- if chr1 = P, then get + or -
         getChar(line, pos, lineLen, chr2);
         if chr2 = '+' then
            command.cmd := powerOn;
         elsif chr2 = '-' then
            command.cmd := powerOff;
         else
            objScreenManager.putError("ERROR: Bad command: 2nd char must be + or -");
            return;
         end if;
      END IF;

      if chr1 /= 'Q' and chr1 /= '.' and chr1 /= 'X' and chr1 /= 'P' then      -- if chr1 not in ('Q'   '.'   'X' 'P') then get n
         get(line(pos..lineLen), n, last);
         pos := last + 1;
         command.n := n;
      end if;

      objScreenManager.putError("");

      CASE Chr1 IS
			when 'P' =>
				null;                                      -- the + and - are processed above
			WHEN 'Q' =>
             command.cmd := quit;
             n := 1;
             nIsInRange := (1 <= n  and n <= 1);
         when 'S' =>                                    -- select or steal
            nIsInRange := (1 <= n  and n <= 9999);
         WHEN 'C' =>
            command.cmd := close;
            nIsInRange := (1 <= n  and n <= kNumSwitches);
         WHEN 'T' =>
            command.cmd := throw;
            nIsInRange := (1 <= n  and n <= kNumSwitches);
         WHEN 'F' =>
            command.cmd := forward;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         WHEN 'R' =>
            command.cmd := Backward;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         WHEN 'V' =>
            command.cmd := Speed;
            nIsInRange := (1 <= n  and n <= kNumTrains);
            get(line(pos..lineLen), command.speed, last);
            pos := last + 1;
         WHEN '.' =>
            command.cmd := halt;
            n := 1;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         WHEN 'H' =>
            command.cmd := horn;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         WHEN 'B' =>
            command.cmd := bell;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         WHEN 'M' =>
            command.cmd := mute;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         WHEN 'L' =>
            command.cmd := light;
            nIsInRange := (1 <= n  and n <= kNumTrains);
         when 'X' =>
            command.cmd := readXML;
            n := 1;
            nIsInRange := (1 <= n  and n <= 1);
            i := 0;
            loop
               getChar(line, pos, lineLen, chr2);
               exit when chr2 = ' ';
               i := i + 1;
               command.fileName(i) := chr2;
            end loop;
            command.fnInUse := i;
         when 'Z' =>
            command.cmd := InitializeLoco;
            nIsInRange := (kFirstPhysAddress <= n  and n <= messageTranslationTypes.kMaxLocoAddress);
            makeEmpty(command.sensors);
            loop
               getChar(line, pos, lineLen, chr2);
               exit when chr2 = ' ';
               pos := pos - 1;
               get(line(pos..lineLen), n, last);
               pos := last + 1;
               addEnd(command.sensors, n);
            end loop;
         WHEN OTHERS =>
            badCommandLetter := true;
      END CASE;
      if badCommandLetter then
         objScreenManager.putError("ERROR: Bad command letter");
      elsif not nIsInRange then
         if command.cmd = halt then
            objScreenManager.putError("ERROR: No trains to halt");
         else
            objScreenManager.putError("ERROR: Integer out of range");
         end if;
      else
         success := true;
      end if;
   exception
      when constraint_error =>
         objScreenManager.putError("CONSTRAINT ERROR: probably a number out of range for a numeric parameter or file name too long");
      when data_error =>
         objScreenManager.putError("DATA ERROR: probably nonnumeric input for a numeric parameter");
      when error : others =>
         objScreenManager.putError("UNKNOWN ERROR: might be a missing command parameter.");
         -- moveCursor(26,1);
         -- put_line(Exception_Information (error));
   end getCommand;

end;