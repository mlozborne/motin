with ada.Text_IO; use ada.Text_IO;
with ada.exceptions; use ada.exceptions;

WITH Screen; USE Screen;

with myStringLibrary; use myStringLibrary;
with logFiles; use logFiles;

PACKAGE BODY ScreenManager IS

   PROTECTED BODY ScreenManagerType IS

      PROCEDURE MyPut(Col : Integer; Row : Integer; S : String) IS
      BEGIN
         MoveCursor(Col,Row);
         Put(S);
      exception
         when error: others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.MyPut --" & kLFString & Exception_Information (error));
            raise;
      END MyPut;

      procedure clearTheScreen is
        str : string(1..screen.screenWidth) := (others => ' ');
      begin
         for i in 1..screen.screenDepth loop
            moveCursor(1,i);
            put(str);
         end loop;
         moveCursor(1,1);
      exception
         when error: others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.ClearTheScreen --" & kLFString & Exception_Information (error));
            raise;
      end clearTheScreen;

      procedure tellUserCntrlC is
      begin
         clearTheScreen;
         moveCursor(1,1);
         put("Type cntrl-c");
      end;

      PROCEDURE Initialize(inConMode : boolean) IS
      BEGIN
         isInControllerMode := inConMode;
         
         MyPut(1,1,"Train PhyAdr/Slot VirAdr/Slot State Speed Dir Light Bell Horn Mute Location");
         MyPut(1,2,"----- ----------- ----------- ----- ----- --- ----- ---- ---- ---- --------");

         MyPut(1,8,"Last msg received: ");

         MyPut(1,10,"SWTICHES");
         MyPut(1,11,"  0 1 2 3 4 5 6 7 8 9");
         MyPut(1,12,"0");
         MyPut(1,13,"1");
         MyPut(1,14,"2");

         MyPut(33,10,"COMMANDS");
         if isInControllerMode then
            MyPut(33,11,"Xf  read XML      Za  s#...s#  initialize loco");
         else
            MyPut(33,12,"SEa select loco   STa steal loco   SXa remove loco");
         end if;
         MyPut(33,13," ");
         MyPut(33,14,"Vt n  velocity (n=0..127)");
         MyPut(33,15,"Cs    close          Ts throw");
         MyPut(33,16,"Ft    forward        Rt reverse       Q  quit");
         MyPut(33,17,"Bt    bell           Ht horn          .  halt all");
         MyPut(33,18,"Lt    light          Mt mute");
         MyPut(33,19,"P+    power on       P- power off");
         MyPut(33,20,"a = phy loco adr  s = switch       s# = sensor#");
         MyPut(33,21,"t = train         f = file name");
         MyPut(1,22, "                                                   ");
         MoveCursor(3,kPromptRow);
         putLineAdminLog(" Initializing ScreenManager");
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.Initialize --" & kLFString & Exception_Information (error));
            raise;
      END Initialize;

       PROCEDURE putTrain(trainId : messageTranslationTypes.trainIdType; 
                          phyadr : natural; physlot : natural;
                          viradr : natural; virslot : natural;
                          state  : string;
                          speed  : natural;
                          direction  : character;
                          light      : string;
                          bell       : string;
                          horn       : string;
                          mute       : string;
                          sensors    : naturalListType) IS
         I    : natural := trainId;
			iter : listIteratorType;
      BEGIN
         IF phyAdr = 0 THEN
            MyPut(2, I+2, ToString80(" "));
            myput (67, i+2, "                      ");
         else
            MyPut(2, I+2,
               toString80(i)(1..6) &                                 -- id
               ToString80(phyadr)(2..6) & "/" &                      -- phyadr
               toString80(physlot)(1..6) &                           -- physlot
               ToString80(viradr)(2..6) & "/" &                      -- viradr
               toString80(virslot)(1..7) &                           -- virslot
               ToString80(state)(1..5) &                             -- state
               ToString80(speed)(2..7) &                             -- speed
               Character'Image(direction)(2..2) & "   " &            -- direction
               ToString80(Light)(1..5) &                             -- light
               ToString80(Bell)(1..5) &                              -- bell
               ToString80(Horn)(1..5) &                              -- horn
               ToString80(Mute)(1..5)                                -- mute
               );
            myput (67, i+2, "                      ");
				iter := moveFront(sensors);
            for j in 1..getCount(Sensors) loop
               myPut (67 + (j-1)*4, i+2, toString80(getCurrent(iter))(1..4));
               iter := moveNext(iter);
            end loop;
         END IF;    
         MoveCursor(3,kPromptRow);
      exception
         when error: others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.PutTrain --" & kLFString & Exception_Information (error));
            raise;
      END PutTrain;  
   
       -- PROCEDURE PutTrains (Trains : in out TrainArrayType) IS
         -- TheDirection : Character;
         -- TheLight, TheBell, TheHorn, TheMute : String(1..3);
      -- BEGIN
         -- FOR I IN 1..kNumTrains LOOP
            -- IF trains(I).phyAdr = 0 THEN
               -- MyPut(2, I+2, ToString80(" "));
               -- myput (67, i+2, "                      ");
            -- else
               -- IF Trains(I).Direction = Forward THEN TheDirection := 'F';
               -- ELSE TheDirection := 'R';
               -- END IF;

               -- IF (Trains(I).Light = On) THEN TheLight := "on ";
               -- ELSE TheLight := "off";
               -- END IF;

               -- IF (Trains(I).Bell = On) THEN TheBell := "on ";
               -- ELSE TheBell := "off";
               -- END IF;

               -- IF (Trains(I).Horn = On) THEN TheHorn := "on ";
               -- ELSE TheHorn := "off";
               -- END IF;

               -- IF (Trains(I).Mute= On) THEN TheMute := "on ";
               -- ELSE TheMute := "off";
               -- END IF;

               -- MyPut(2, I+2,
                  -- toString80(i)(1..6) &                                            -- id
                  -- ToString80(trains(i).phyadr)(2..6) & "/" &                       -- phyadr
                  -- toString80(trains(i).physlot)(1..6) &                            -- physlot
                  -- ToString80(Trains(I).viradr)(2..6) & "/" &                       -- viradr
                  -- toString80(trains(i).virslot)(1..7) &                            -- virslot
                  -- ToString80(Trains(I).state)(1..5) &                              -- state
                  -- ToString80(Trains(I).Speed)(2..7) &                              -- speed
                  -- Character'Image(TheDirection)(2..2) & "   " &                    -- direction
                  -- ToString80(TheLight)(1..5) &                                     -- light
                  -- ToString80(TheBell)(1..5) &                                      -- bell
                  -- ToString80(TheHorn)(1..5) &                                      -- horn
                  -- ToString80(TheMute)(1..5)                                        -- mute
                  -- );
               -- myput (67, i+2, "                      ");
               -- moveFront(trains(i).sensors);
               -- for j in 1..getCount(trains(i).Sensors) loop
                  -- myPut (67 + (j-1)*4, i+2, toString80(getCurrent(trains(i).sensors))(1..4));
                  -- moveNext(trains(i).sensors);
               -- end loop;
            -- END IF;
         -- END LOOP;
         -- MoveCursor(3,kPromptRow);
      -- exception
         -- when error: others =>
            -- put_line("UNPLANNED EXCEPTION in ScreenManager.PutTrains --" & kLFString & Exception_Information (error));
            -- raise;
      -- END PutTrains;  
   
      PROCEDURE PutMessage (str1 : string; str2 : String) IS
      begin
         MyPut(20, kMessageRow, toString80(str2)(1..60));
         MyPut(18, kMessageRow+1, toString80(str1)(1..61));
         MoveCursor(3,kPromptRow);
         putLineAdminLog(" Received: " & str2);
         putLineAdminLog("                " & str1);
      exception
         when error: others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.PutMessage --" & kLFString & Exception_Information (error));
            raise;
      END PutMessage;

      PROCEDURE PutSwitches (str : string) IS
         Str80 : String80;
         numSw : integer := str'length;
      BEGIN
         Str80 := ToString80(" ");
         FOR I IN 1..9 LOOP
            Str80 (2*I+1) := str(i);
         END LOOP;
         MyPut(3, kSwitchesRow, Str80 (1..20));

         Str80 := ToString80(" ");
         FOR I IN 10..19 LOOP
            Str80 (2*(I-10)+1) := str(i);
         END LOOP;
         MyPut(3, kSwitchesRow+1, Str80 (1..20));

         Str80 := ToString80(" ");
         FOR I IN 20..numSw LOOP
            Str80 (2*(I-20)+1) := str(i);
         END LOOP;
         MyPut(3, kSwitchesRow+2, Str80 (1..20));
         MoveCursor(3,kPromptRow);
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.PutSwitches --" & kLFString & Exception_Information (error));
            raise;
      END PutSwitches;

      -- PROCEDURE PutSwitches (Switches : SwitchArrayType) IS
         -- Str80 : String80;
         -- Chr : Character;
      -- BEGIN
         -- Str80 := ToString80(" ");
         -- FOR I IN 1..9 LOOP
            -- IF Switches(I) = Thrown THEN Chr := 'T';
            -- ELSIF Switches(I) = Closed THEN Chr := 'C';
            -- ELSif switches(i) = beginThrown or switches(i) = beginClosed then Chr := '*';
            -- elsif switches(i) = Unknown then Chr := '?';
            -- END IF;
            -- Str80 (2*I+1) := Chr;
         -- END LOOP;
         -- MyPut(3, kSwitchesRow, Str80 (1..20));

         -- Str80 := ToString80(" ");
         -- FOR I IN 10..19 LOOP
            -- IF Switches(I) = Thrown THEN Chr := 'T';
            -- ELSIF Switches(I) = Closed THEN Chr := 'C';
            -- ELSif switches(i) = beginThrown or switches(i) = beginClosed then Chr := '*';
            -- elsif switches(i) = Unknown then Chr := '?';
            -- END IF;
            -- Str80 (2*(I-10)+1) := Chr;
         -- END LOOP;
         -- MyPut(3, kSwitchesRow+1, Str80 (1..20));

         -- Str80 := ToString80(" ");
         -- FOR I IN 20..26 LOOP
            -- IF Switches(I) = Thrown THEN Chr := 'T';
            -- ELSIF Switches(I) = Closed THEN Chr := 'C';
            -- ELSif switches(i) = beginThrown or switches(i) = beginClosed then Chr := '*';
            -- elsif switches(i) = Unknown then Chr := '?';
            -- END IF;
            -- Str80 (2*(I-20)+1) := Chr;
         -- END LOOP;
         -- MyPut(3, kSwitchesRow+2, Str80 (1..20));
         -- MoveCursor(3,kPromptRow);
      -- exception
         -- when error : others =>
            -- put_line("UNPLANNED EXCEPTION in ScreenManager.PutSwitches --" & kLFString & Exception_Information (error));
            -- raise;
      -- END PutSwitches;

      PROCEDURE PutError (Error : String) IS
      BEGIN
         MyPut(1, kErrorRow, toString80(error));
         MoveCursor(3,kPromptRow);
         if error'last /= 0 then
            putLineAdminLog("   Action: " & error(error'first..error'last));
         end if;
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.PutError --" & kLFString & Exception_Information (error));
            raise;
      END PutError;

      PROCEDURE PutPrompt (Prompt : String) IS
      BEGIN
         myput(1, kPromptRow,toString80(" ")(1..70));
         MyPut(1, kPromptRow, prompt);
         MoveCursor(2 + prompt'last,kPromptRow);
      exception
         when error : others =>
            put_line("UNPLANNED EXCEPTION in ScreenManager.PutPrompt --" & kLFString & Exception_Information (error));
            raise;
      END PutPrompt;

      -- PROCEDURE PutException (str : String) IS     -- don't use this one
      -- BEGIN
         -- movecursor(1,kExceptionRow);
         -- for i in 1..ExcepRowInc loop
            -- new_line;
         -- end loop;
         -- ExcepRowInc := ExcepRowInc + 4;
         -- put_line(str);
         -- if is_open(adminLog) then
            -- put_line(adminLog, durationAsString(clock - startTime) & "   " & "Exception");
            -- put_line(adminLog, str);
         -- end if;
      -- exception
         -- when error: others =>
            -- put_line("EXCEPTION in ScreenManager.PutException --" & kLFString & Exception_Information (error));
            -- raise;
      -- END PutException;

   END ScreenManagerType;
END ScreenManager;


