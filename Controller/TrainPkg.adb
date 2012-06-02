-- Package Body for TrainPkg
-- Stores information about trains in TrainTask
-- Keeps a list of TrainTasks
-- Added 4/26/2011 By Alex Eveleth

WITH Globals, CommandQueueManager, LayoutPkg, MessageTranslationLibrary;
WITH Ada.Exceptions, Ada.Text_IO, Interfaces;
USE Globals, CommandQueueManager, LayoutPkg, MessageTranslationLibrary;
USE Ada.Exceptions, Ada.Text_IO, Interfaces;
with MessageTranslationTypes; use messageTranslationTypes;


PACKAGE BODY TrainPkg IS
   PROCEDURE SpawnTrain (
         TrainId    : TrainIdType;
         TrainState : TrainStateType := Halted;
         Direction  : DirectionType  := Forward;
         Speed      : SpeedType      := 0) IS
      TrainTaskPtr : TrainTaskAccess;
   BEGIN
      TrainTaskPtr := NEW TrainTask(TrainId);
      --use TrainIdQueueList procedure AddTrain in Spawn train, may need to change AddTrain
      TrainTaskPtr.SetState(TrainState);
      TrainTaskPtr.SetDirection(Direction);
      TrainTaskPtr.SetSpeed(Speed);
      Lists.AddToEnd(TrainTaskList, TrainTaskPtr);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("**************** EXCEPTION in TrainPkg:SpawnTrain");
         raise;
   END SpawnTrain;
   


   PROCEDURE SendToOutQueue (
         Cmd : MessageType) IS
   BEGIN
      myPutLine("        ........." & toEnglish(cmd) & "  TrainPkg in SendToOutQu ");
      CommandQueueManager.OutQueue.putMessage(Cmd);
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("**************** EXCEPTION in TrainPkg:SendToOutQueue");
         raise;
   END SendToOutQueue;

   PROCEDURE SetLayout (
         L : LayoutManagerAccess) IS
   BEGIN
      LayoutPtr := L;
   EXCEPTION
      WHEN Error : OTHERS =>
         put_line("**************** EXCEPTION in TrainPkg:SetLayout" );
         raise;
   END SetLayout;

   TASK BODY TrainTask IS
      State     : TrainStateType;
      Direction : DirectionType;
      Speed     : SpeedType;
      Light     : OnOffType      := Off;
      Bell      : OnOffType      := Off;
      Horn      : OnOffType      := Off;
      Mute      : OnOffType      := Off;
      ThrowNext : OnOffType      := Off;
      CloseNext : OnOffType      := Off;
      Queue     : QueuePtr;
      Cmd       : MessageType;
      -- T         : Integer := 0;                                       -- mo 12/29/11
   BEGIN
      DECLARE
         Result : Boolean;
      BEGIN
         -- If train is not already in TrainIdQueueList then add it
         CommandQueueManager.TrainIdQueueList.HasTrain(TrainId, Result);
         IF NOT Result THEN
            CommandQueueManager.TrainIdQueueList.AddTrain(TrainId);
         END IF;
      END;
      CommandQueueManager.TrainIdQueueList.GetQueue(TrainId, Queue);
      
      ACCEPT SetState (TrainState : IN TrainStateType) DO
         State := TrainState;
      END;
      
      ACCEPT SetDirection (Dir : IN DirectionType) DO
         Direction := Dir;
      END;
      
      ACCEPT SetSpeed (Spd : IN SpeedType) DO
         Speed := Spd;
      END;

      LOOP
         BEGIN
            IF false THEN   
               DELAY 0.01; -- Wait so that other tasks have a chance to run     test 3              
            else
               Queue.GetMessage(Cmd);
               myPutLine("        ........... " & toEnglish(cmd) & "  received in TrainTask" & TrainIdType'Image(TrainId));
               CASE Cmd.ByteArray(1) IS
                  WHEN OPC_LOCO_SPD =>
                     -- Set the Speed
                     DECLARE
                        SlotNum : SlotType;
                        Spd     : SpeedType;
                     BEGIN
                        SplitLocoSpdMsg(Cmd, SlotNum, Spd);
                        Speed := Spd;
                     END;
                     IF Speed = 0 or speed = 1 THEN
                        CASE State IS
                           WHEN Moving =>
                              State := BeginHalted;
                              
                              if speed = 0 then                                            -- mo 1/9/12               
                                 SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedSlowStop));
                              else
                                 speed := 0;
                                 SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedAbruptStop));
                              end if;
                              
                              SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                              delay WaitTime;                                    -- mo 12/29/11
                              State := Halted;                                   -- mo 12/29/11;
                              myPutLine("        .........call LayoutPtr.ReleaseReservation(TrainId) in TrainPkg"); 
                              LayoutPtr.ReleaseReservation(TrainId);             -- mo 12/29/11
                              SendToOutQueue(makePutTrainStateMsg(TrainId, State)); -- mo 12/29/11               
                          WHEN Waiting =>
                              State := Halted;
                              SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                           WHEN OTHERS =>  --A
                              myPutLine("        ............. not moving/waiting so no action" & "    in TrainTask" & TrainIdType'Image(TrainId));
                        END CASE;
                     ELSE
                        CASE State IS
                           WHEN Halted =>                                    -- mo 12/29/11
                              DECLARE
                                 Result : Boolean;
                              BEGIN
                                 myPutLine("        .........call LayoutPtr.MakeReservation(TrainId, Result) in TrainPkg"); 
                                 LayoutPtr.MakeReservation(TrainId, Result);
                                 IF Result THEN
                                    SendToOutQueue(makeLocoSpdMsg(TrainId, Speed));
                                    State := Moving;
                                    SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                 ELSE
                                    state := waiting;                             -- mo 12/29/11  
                                    myPutLine("        .........call LayoutPtr.ReleaseReservation(TrainId) in TrainPkg"); 
                                    LayoutPtr.ReleaseReservation(TrainId);        -- mo 12/29/11                                    
                                    SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                 END IF;
                              END;
                           WHEN Moving =>
                              SendToOutQueue(makeLocoSpdMsg(TrainId, Speed));
                           WHEN OTHERS => -- B
                              myPutLine("        ............. not moving/halted so no action" & "    in TrainTask" & TrainIdType'Image(TrainId));
                        END CASE;
                     END IF;
                     SendToOutQueue(makePutTrainInformationMsg(TrainId, Speed, Direction, Light, Bell, Horn, Mute));          -- mo 1/9/12
                  WHEN OPC_LOCO_DIRF =>
                     -- Set the Direction, Light, Bell, and/or horn
                     DECLARE
                        SlotNum     : SlotType;
                        Dir         : DirectionType;
                        L           : OnOffType;
                        H           : OnOffType;
                        B           : OnoffType;
                        SendDirfMsg : Boolean       := true;
                     BEGIN
                        SplitLocoDirfMsg(Cmd, SlotNum, Dir, L, H, B);
                        IF Light /= L THEN
                           Light := L;
                        END IF;
                        IF Bell /= B THEN
                           Bell := B;
                        END IF;
                        IF Horn /= H THEN
                           Horn := H;
                        END IF;
                        IF Dir /= Direction THEN
                           myPutLine("        .......... Changing Direction    in TrainTask: ");
                           Direction := Dir;
                           CASE State IS
                              WHEN Waiting =>
                                 myPutLine("        .........call LayoutPtr.ChangeDirection(TrainId) in TrainPkg"); 
                                 LayoutPtr.ChangeDirectionOf(TrainId);
                                 SendToOutQueue(makeLocoDirfMsg(TrainId, Direction, Light, Horn, Bell));  -- mo 12/29/11
                                 SendDirfMsg := false;                                                -- mo 1/3/12
                                 DECLARE
                                    Result : Boolean;
                                 BEGIN
                                    myPutLine("        .........call LayoutPtr.MakeReservation(TrainId, Result) in TrainPkg"); 
                                    LayoutPtr.MakeReservation(TrainId, Result);
                                    IF Result THEN
                                       State := Moving;
                                       SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                       SendToOutQueue(makeLocoSpdMsg(TrainId, Speed));
                                    END IF;
                                 END;
                              WHEN Moving  =>                                                  -- mo 12/29/11
                                 SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedSlowStop));        -- mo 1/9/12
                                 State := BeginChangeDirection;
                                 SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                 delay WaitTime;
                                 myPutLine("        .........call LayoutPtr.ChangeDirection(TrainId) in TrainPkg"); 
                                 LayoutPtr.ChangeDirectionOf(TrainId);
                                 SendToOutQueue(makeLocoDirfMsg(TrainId, Direction, Light, Horn, Bell));  -- mo 12/29/11
                                 SendDirfMsg := false;                                                -- mo 1/3/12
                                    DECLARE
                                       Result : Boolean;
                                    BEGIN
                                       myPutLine("        .........call LayoutPtr.MakeReservation(TrainId, Result) in TrainPkg"); 
                                       LayoutPtr.MakeReservation(TrainId, Result);
                                       IF Result THEN
                                          State := Moving;
                                          SendToOutQueue(makeLocoSpdMsg(TrainId, Speed));
                                          SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                       ELSE
                                          State := Waiting;
                                          SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                          myPutLine("        .........call LayoutPtr.ReleaseReservation(TrainId) in TrainPkg"); 
                                          LayoutPtr.ReleaseReservation(TrainId);
                                       END IF;
                                    END;
                              WHEN Halted =>
                                 myPutLine("        .........call LayoutPtr.ChangeDirection(TrainId) in TrainPkg"); 
                                 LayoutPtr.ChangeDirectionOf(TrainId);
                                 SendToOutQueue(makeLocoDirfMsg(TrainId, Direction, Light, Horn, Bell));  -- mo 12/29/11
                                 SendDirfMsg := false;                                                -- mo 1/3/12
                              WHEN OTHERS => -- C
                                 myPutLine("        ............. not halted/waiting/moving so no action" & "    in TrainTask" & TrainIdType'Image(TrainId));
                           END CASE;
                        END IF;
                        IF SendDirfMsg THEN
                           SendToOutQueue(makeLocoDirfMsg(TrainId, Direction, Light, Horn, Bell));
                        END IF;
                        SendToOutQueue(makePutTrainInformationMsg(TrainId, Speed, Direction, Light, Bell, Horn, Mute));
                     END;
                  WHEN OPC_LOCO_SND =>
                     -- Set the Sound (and other things)
                     DECLARE
                        SlotNum : SlotType;
                        Fun5    : OnOffType;
                        Fun6    : OnOffType;
                        M       : OnOffType;
                        SendMsg : Boolean   := False;
                     BEGIN
                        SplitLocoSndMsg(Cmd, SlotNum, Fun5, Fun6, M);
                        IF CloseNext /= Fun5 THEN
                           CloseNext := Fun5;
                           myPutLine("        .........call LayoutPtr.MoveNextSwitch(TrainId, Closed) in TrainPkg"); 
                           LayoutPtr.MoveNextSwitch(TrainId, Closed);
                           SendMsg := True;
                        ELSIF ThrowNext /= Fun6 THEN
                           ThrowNext := Fun6;
                           myPutLine("        .........call LayoutPtr.MoveNextSwitch(TrainId, Thrown) in TrainPkg"); 
                           LayoutPtr.MoveNextSwitch(TrainId, Thrown);
                           SendMsg := True;
                        END IF;
                        IF Mute /= M THEN
                           Mute := M;
                           SendMsg := True;
                        END IF;
                        IF SendMsg THEN 
                           SendToOutQueue(makeLocoSndMsg(TrainId, Mute));
                           SendToOutQueue(makePutTrainInformationMsg(TrainId, Speed, Direction, Light, Bell, Horn, Mute));
                        END IF;
                     END;
                  when OPC_SW_REQ =>
                     -- This is a request for the train to forward the message. 
                     -- Thereby, introducting a delay.
                     CommandQueueManager.put(cmd);
                  WHEN UZero =>
                     -- Extended Messages
                     IF Cmd.Size >= 2 THEN
                        CASE Cmd.ByteArray(2) IS 
                        
                           when msgTrainTaskQuit =>               -- this will end the train task
                              sendToOutQueue(makeLocoSpdMsg(trainId, kSpeedAbruptStop)); 
                              exit;
                           when MsgReinitializeTrain =>              -- mo 1/28/12
                              state := halted;
                              speed := 0;
                              direction := forward;
                              light := off;
                              horn := off;
                              bell := off;
                              mute := off;
                              sendToOutQueue(makeLocoSpdMsg(trainId, kSpeedAbruptStop)); 
                              SendToOutQueue(makeLocoDirfMsg(TrainId, Forward, Off, Off, Off));
                              SendToOutQueue(makeLocoSndMsg(TrainId, Off));
                              SendToOutQueue(makePutTrainInformationMsg(TrainId, 0, Forward, Off, Off, Off, Off));
                              SendToOutQueue(makePutTrainStateMsg(TrainId, State));                              
                           WHEN MsgFrontSensorFired =>
                              CASE State IS
                                 WHEN Moving =>
                                    DECLARE
                                       Result : Boolean;
                                    BEGIN
                                       myPutLine("        .........call LayoutPtr.MakeReservation(TrainId, Result) in TrainPkg"); 
                                       LayoutPtr.MakeReservation(TrainId, Result);
                                       IF NOT Result THEN
                                          State := BeginWaiting;
                                          SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedSlowStop));
                                          SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                          delay waitTime;                                    -- mo 12/29/11
                                          State := Waiting;                                  -- mo 12/29/11
                                          myPutLine("        .........call LayoutPtr.ReleaseReservation(TrainId) in TrainPkg"); 
                                          LayoutPtr.ReleaseReservation(TrainId);             -- mo 12/29/11
                                          SendToOutQueue(makePutTrainStateMsg(TrainId, State)); -- mo 12/29/11
                                       END IF;
                                    END;
                                 WHEN OTHERS => -- D
                                    myPutLine("        ............. not moving, no action" & "    in TrainTask" & TrainIdType'Image(TrainId));
                              END CASE;
                           WHEN MsgBackSensorFired =>
                              null;
                           WHEN MsgSensorError =>
                              CASE State IS
                                 WHEN Error =>
                                    NULL;
                                 WHEN OTHERS =>
                                    State := Error;
                                    SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                    SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedAbruptStop));      -- mo 1/9/12
                              END CASE;
                           WHEN MsgLoseReservation =>
                              IF State = Moving THEN
                                 State := BeginWaiting;
                                 SendToOutQueue(makeLocoSpdMsg(TrainId, kSpeedSlowStop));        -- mo 1/9/12
                                 SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                 delay waitTime;                                    -- mo 12/29/11
                                 State := Waiting;                                  -- mo 12/29/11
                                 myPutLine("        .........call LayoutPtr.ReleaseReservation(TrainId) in TrainPkg"); 
                                 LayoutPtr.ReleaseReservation(TrainId);             -- mo 12/29/11
                                 SendToOutQueue(makePutTrainStateMsg(TrainId, State)); -- mo 12/29/11
                              END IF;
                           WHEN MsgTryToMoveAgain =>
                              CASE State IS
                                 WHEN Waiting =>                        -- mo 12/29/11
                                    DECLARE
                                       Result : Boolean;
                                    BEGIN
                                       myPutLine("        .........call LayoutPtr.MakeReservation(TrainId, Result) in TrainPkg"); 
                                       LayoutPtr.MakeReservation(TrainId, Result);
                                       IF Result THEN
                                          State := Moving;
                                          SendToOutQueue(makeLocoSpdMsg(TrainId, Speed));
                                          SendToOutQueue(makePutTrainStateMsg(TrainId, State));
                                       END IF;
                                    END;
                                 WHEN OTHERS => -- E
                                    myPutLine("        ............. not waiting so no action" & "    in TrainTask" & TrainIdType'Image(TrainId));
                              END CASE;
                           WHEN OTHERS => -- F
                              myPutLine("        ............. unknown message 1" & "    in TrainTask" & TrainIdType'Image(TrainId));
                        END CASE;
                     END IF;
                  WHEN 16#ff# =>
                     EXIT; -- for testing
                  WHEN OTHERS => -- G
                     myPutLine("        ............. unknown message 2" & "    in TrainTask" & TrainIdType'Image(TrainId));
               END CASE;
            END IF;                       
         EXCEPTION
            WHEN Error : OTHERS =>
               put_line("**************** EXCEPTION in TrainTask" & Integer'Image(TrainId) & " : " & Exception_Information(Error));
         END;
      END LOOP;
   END TrainTask;
END TrainPkg;
