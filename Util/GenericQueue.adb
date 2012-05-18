--4/2/6/2011
--generic queue package

PACKAGE BODY GenericQueue IS
--------------------------------------------------------------------
   procedure Enqueue (Q: IN OUT Queue; E: IN QueueElement) IS  
   BEGIN
      Lists.AddToEnd(Q.Store, E);
   END Enqueue;

   procedure Dequeue (Q: IN OUT Queue; E: OUT QueueElement) IS  
   BEGIN
      E := Lists.RetrieveFront(Q.Store);
      Lists.RemoveFront(Q.Store);
   END Dequeue;

   function IsEmpty (Q: IN Queue) return boolean IS 
   BEGIN
      RETURN Lists.IsEmpty(Q.Store);
   END IsEmpty;
   
   procedure makeEmpty(Q: in out queue) is
   begin 
      Lists.makeEmpty(Q.store);
   end makeEmpty;
END GenericQueue;
--------------------------------------------------------------------
