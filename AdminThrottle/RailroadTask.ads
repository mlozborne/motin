package RailroadTask is

   TASK TYPE RailroadTaskType IS
      ENTRY Start;
   END;
   
   objRailroadTask : railroadTaskType;
private
   kLFString              : string(1..1) := ( 1=> standard.ascii.LF);
end;

