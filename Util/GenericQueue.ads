--4/26/2011
--generic queue package
--reference: Ada 95 Feldman/Koffman

WITH GenericList;

GENERIC
   TYPE QueueElement IS PRIVATE;
PACKAGE GenericQueue IS
   TYPE Queue IS LIMITED PRIVATE;                          
   QueueEmpty: EXCEPTION;                                  

      procedure Enqueue  (Q: IN OUT Queue; E: IN QueueElement);  
      procedure Dequeue  (Q: IN OUT Queue; E: OUT QueueElement);   
      function IsEmpty   (Q: IN Queue) return boolean;
      procedure makeEmpty(Q: in out queue);      

PRIVATE
      PACKAGE Lists IS NEW GenericList(ElementType=>QueueElement);
      TYPE Queue IS RECORD
         Store :Lists.List;
      END RECORD;
END GenericQueue;
---------------------------------------------------------------------------
