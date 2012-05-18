WITH Ada.Calendar; USE Ada.Calendar;

package KeyboardTask is

   TASK TYPE KeyboardTaskType IS
      ENTRY Start(inConMode : boolean);
   END;

   ObjKeyboardTask : KeyboardTaskType;

private
   kLFString              : string(1..1) := ( 1=> standard.ascii.LF);
   StartTime              : Time := Clock;
   elapsedDuration        : duration := 0.0;   

end;