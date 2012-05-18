--4/26/2011
--generic list package
--reference: Ada 95 Feldman/Koffman

GENERIC
   TYPE ElementType IS PRIVATE;
PACKAGE GenericList IS
   TYPE List IS LIMITED PRIVATE;
   ListEmpty                : EXCEPTION;
   noCurrentElementInList   : exception;

   procedure makeEmpty(L : in out List);
   FUNCTION IsEmpty(L :IN List) RETURN Boolean;
   FUNCTION RetrieveFront (L: IN List) RETURN ElementType;
   
   PROCEDURE RemoveFront (L: IN OUT List);
   -- pre   : none
   -- raise : ListEmpty if list is empty
   -- post  : if list not empty removes first node; current := null
   
   PROCEDURE AddToEnd (L: IN OUT List; Element: IN ElementType);
   -- pre   : none
   -- post  : new node added to end of list; current := null
   
   function hasCurrent(L: in list) return boolean;
   -- pre   : none
   -- return: true if current /= null
   procedure moveFirst(l: in out list);
   -- pre   : none
   -- post  : current := head
   procedure moveNext (L: in out list);
   -- pre   : none
   -- post  : current := null or current.next
   function retrieveCurrent(L: in list) return elementType;
   -- pre   : has current
   -- raise : noCurrentElementInList
   -- return: current.element
   procedure removeCurrent(L: in out list);
   -- pre   : has current
   -- raise : noCurrentElementInList
   -- post  : current := null
PRIVATE

   kLFString        : string(1..1) := ( 1=> standard.ascii.LF);

   TYPE node;
   TYPE nodePtr IS ACCESS node;
   TYPE node IS RECORD
      Element: ElementType;
      Next   : nodePtr;
   END RECORD;

   TYPE List IS RECORD
      Head     : nodePtr := null;
      Tail     : nodePtr := null;
      previous : nodePtr := null;
      current  : nodePtr := null;
   END RECORD;

END GenericList;

