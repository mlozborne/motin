--4/26/2011
--generic list package
--reference: Ada 95 Feldman/Koffman
WITH Ada.Unchecked_Deallocation;
with ada.exceptions; use ada.exceptions;
with ada.text_io; use ada.text_io;

PACKAGE BODY GenericList IS

   PROCEDURE DisposeListNode IS
      NEW Ada.Unchecked_Deallocation (Object=>node, Name=>nodePtr);

   procedure makeEmpty(L : in out List) is
      cr, nx : nodePtr;
   begin
      cr := L.head;
      while cr /= null loop
         nx := cr.next;
         DisposeListNode(cr);
         cr := nx;
      end loop;
      L.head := null;
      L.tail := null;
      L.previous := null;
      L.current := null;
   end makeEmpty;
         
   FUNCTION IsEmpty(L : IN List) RETURN Boolean IS
   BEGIN
      IF L.Head = NULL THEN
         RETURN True;
      ELSE
         RETURN False;
      END IF;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.IsEmpty --" & kLFString & Exception_Information (error));
         raise;
   END IsEmpty;

   FUNCTION RetrieveFront (L: IN List) RETURN ElementType IS
   BEGIN
      IF L.Head = NULL THEN
         RAISE ListEmpty;
      ELSE
         RETURN L.Head.Element;
      END IF;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.RetrieveFront --" & kLFString & Exception_Information (error));
         raise;
   END RetrieveFront;

   PROCEDURE RemoveFront (L: IN OUT List) IS
      Temp: nodePtr;
   BEGIN
      L.current := null;
      L.previous := null;
      IF L.Head = NULL THEN
         RAISE ListEmpty;
      ELSE
         if L.head = L.tail then
            L.tail := null;
         end if;
         Temp := L.Head;
         L.Head := L.Head.Next;
         -- DisposeListNode(X=> Temp);   -- memory leak
      END IF;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.RemoveFront --" & kLFString & Exception_Information (error));
         raise;
   END RemoveFront;

   PROCEDURE AddToEnd (L: IN OUT List; Element: IN ElementType) IS
      ptr  : nodePtr;
   BEGIN
      L.current := null;
      L.previous := null;
      ptr := new node'(element, null);
      IF L.Head = NULL THEN
         L.Head := ptr;
      ELSE
         L.tail.next := ptr;
      END IF;
      L.Tail := ptr;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.AddEnd --" & kLFString & Exception_Information (error));
         raise;
   END AddToEnd;
   
   function hasCurrent(L: in list) return boolean is
   begin
      return l.current /= null;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.hasCurrent --" & kLFString & Exception_Information (error));
         raise;
   end;
   
   procedure moveFirst(l: in out list) is
   begin
      l.previous := null;
      l.current := l.head;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.moveFirst --" & kLFString & Exception_Information (error));
         raise;
   end moveFirst;
   
   procedure moveNext (L: in out list) is
   begin
      l.previous := l.current;
      if l.current /= null then
         l.current := l.current.next;
      end if;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.moveNext --" & kLFString & Exception_Information (error));
         raise;
   end moveNext;
   
   function retrieveCurrent(L: in list) return elementType is
   begin
      if not hasCurrent(l) then
         raise noCurrentElementInList;
      else
         return l.current.element;
      end if;
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.retrieveCurrent --" & kLFString & Exception_Information (error));
         raise;
   end retrieveCurrent;
   
   procedure removeCurrent(L: in out list) is 
      temp  : nodePtr := l.current;
   begin
      if not hasCurrent(l) then         -- no nodes or no current established
         raise noCurrentElementInList;
      elsif l.head = l.tail then        -- one node
         l.head := null;
         l.tail := null;
      elsif l.head = l.current then     -- two or more nodes, get rid of first one
         l.head := l.current.next;
      elsif l.tail = l.current then     -- two or more nodes, get rid of last one
         l.previous.next := l.current.next;
         l.tail := l.previous;
      else                              -- three or more nodes, get rid of interior one
         l.previous.next := l.current.next;
      end if;
      l.previous := null;
      l.current := null;
      -- DisposeListNode(temp);    -- memory leak
   exception
	   when error : others =>
		   put_line("**************** UNPLANNED EXCEPTION in GenericList.removeCurrent --" & kLFString & Exception_Information (error));
         raise;
  end removeCurrent;
            
END GenericList;

