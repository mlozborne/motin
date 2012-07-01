WITH LocoBuffer; USE LocoBuffer;
with Tracer; use Tracer;
with Ada.Command_Line; use Ada.Command_Line;

procedure StartLocoBufferServer is
   ListenForLocoBufferClientsTask       : ListenForLocoBufferClientsTaskType;
   WriteLocoBufferStringTask            : WriteLocoBufferStringTaskType;
   ReadLocoBufferByteTask               : ReadLocoBufferByteTaskType;
BEGIN
    for arg in 1..argument_count loop
        if stringToLower(argument(arg)) = "trace" 
           and then arg + 1 <= argument_count then
            withTrace := (stringToLower(argument(arg+1)) = "yes");
        end if;
    end loop;    
end StartLocoBufferServer;
