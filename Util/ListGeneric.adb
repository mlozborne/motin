WITH Unchecked_Deallocation;
with ada.exceptions; use ada.exceptions;

PACKAGE BODY ListGeneric IS

   PROCEDURE Dispose IS NEW Unchecked_Deallocation (Object => NodeType, Name => NodePtrType);

   PROCEDURE Initialize (L : IN OUT ListType) IS
	-- pre   list contains no nodes
	-- post  list contains a sentinel node, count is 0, curPos is null
      Ptr : NodePtrType;
   BEGIN
      -- Create sentinel node
      Ptr := NEW NodeType;
      Ptr.Prev := Ptr;
      Ptr.Next := Ptr;
      L.Sentinel := Ptr;
      L.count := 0;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.Initialize --" & kLFString & Exception_Information (error));
         raise;
   END Initialize;

	procedure removeSentinel(L : in out listType) is
	-- pre    sentinel is only node left in list
	-- post   list contains no nodes
	begin
      Dispose(L.Sentinel);
      L.Sentinel := Null;
      L.count := 0;
	end removeSentinel;

   PROCEDURE MakeEmpty (L : IN OUT ListType) IS
		curPos, p	: nodePtrType;
   BEGIN
		if L.count = 0 then
			return;
		end if;
		curPos := L.sentinel.next;
		while curPos /= L.sentinel loop
			p := curPos;
			curPos := curPos.next;
			dispose(p);
		end loop;
		removeSentinel(L);
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.MakeEmpty --" & kLFString & Exception_Information (error));
         raise;
   END MakeEmpty;

   procedure copyFromTo(source : in listType; dest : in out listType) is
      ps, pd, pn : nodePtrType;   -- pointers to source, destination, new
   begin
      makeEmpty(dest);

		if source.count = 0 then
			return;
	   end if;

      initialize(dest);
		ps:= source.sentinel.next;
		pd := dest.sentinel;
		while ps /= source.sentinel loop
			pn := new nodeType'(ps.e, pd, dest.sentinel);
			pd.next := pn;
			dest.sentinel.prev := pn;
			pd := pn;
			ps := ps.next;
		end loop;
		dest.count := source.count;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.copyFromTo --" & kLFString & Exception_Information (error));
         raise;
   end copyFromTo;

   FUNCTION IsEmpty (L : ListType) RETURN Boolean IS
   BEGIN
      RETURN L.count = 0;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.IsEmpty --" & kLFString & Exception_Information (error));
         raise;
    END IsEmpty;

   FUNCTION IsNull (I : listIteratorType) RETURN Boolean is
   BEGIN
      RETURN I.ptr = null;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.IsNull --" & kLFString & Exception_Information (error));
         raise;
   END IsNull;

   procedure ReverseList (L : in out listType) is
      copy              : ListType;
		ptrL              : nodePtrType; -- ptr to node in L
		ptrC              : nodePtrType; -- ptr to node in copy
		ptrN              : nodePtrType; -- ptr to new node
   begin
		if L.count = 0 then
			return;
	   end if;

      initialize(copy);
		ptrL:= L.sentinel.next;                -- first data node in L
		ptrC := copy.sentinel;                 -- sentinel in copy
		while ptrL /= L.sentinel loop
			ptrN := new nodeType'(ptrL.e, copy.sentinel, ptrC);
			ptrC.prev := ptrN;
			copy.sentinel.next := ptrN;
			ptrC := ptrN;
			ptrL := ptrL.next;
		end loop;
		makeEmpty(L);
		L := copy;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.ReverseList --" & kLFString & Exception_Information (error));
         raise;
   end ReverseList;

   PROCEDURE AddFront (L : IN OUT ListType; E : IN ElementType) IS
      Ptr, Sent : NodePtrType;
   BEGIN
      IF L.count = 0 THEN
         Initialize(L);
      END IF;

      Sent := L.Sentinel;
      Ptr := NEW NodeType'(E, Sent, Sent.Next);
      Sent.Next.Prev := Ptr;
      Sent.Next := Ptr;

      L.count := L.count + 1;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.AddFront --" & kLFString & Exception_Information (error));
         raise;
   END AddFront;

   PROCEDURE AddEnd (L : IN OUT ListType; E : IN ElementType) IS
      Ptr, Sent : NodePtrType;
   BEGIN
      IF L.count = 0 THEN
         Initialize(L);
      END IF;

      Sent := L.Sentinel;
      Ptr := NEW NodeType'(E, Sent.Prev, Sent);
      Sent.Prev.Next := Ptr;
      Sent.Prev := Ptr;

      L.count := L.count + 1;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.AddEnd --" & kLFString & Exception_Information (error));
         raise;
   END AddEnd;

   PROCEDURE RemoveFront (L : IN OUT ListType) IS
      Ptr, Sent : NodePtrType;
   BEGIN
      IF L.count = 0 THEN
         RAISE ListEmpty;
      END IF;

      Sent := L.Sentinel;
      Ptr := Sent.Next;
      Sent.Next := Ptr.Next;
      Ptr.Next.Prev := Sent;
      Dispose(Ptr);
		L.count := L.count - 1;
		if L.count = 0 then
			removeSentinel(L);
      END IF;
   exception
      when listEmpty =>
         raise;
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.RemoveFront --" & kLFString & Exception_Information (error));
         raise;
   END RemoveFront;

   PROCEDURE RemoveEnd (L : IN OUT ListType) IS
      Ptr, Sent : NodePtrType;
   BEGIN
      IF IsEmpty (L) THEN
         RAISE ListEmpty;
      END IF;

      Sent := L.Sentinel;
      Ptr := Sent.Prev;
      Sent.Prev := Ptr.Prev;
      Ptr.Prev.Next := Sent;
      Dispose(Ptr);
		L.count := L.count - 1;
		if L.count = 0 then
			removeSentinel(L);
      END IF;
   exception
      when listEmpty =>
         raise;
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.RemoveEnd --" & kLFString & Exception_Information (error));
         raise;
   END RemoveEnd;
	
	procedure RemoveElement(L : in out ListType; E : in ElementType) is
		ptr : nodePtrType;
	begin
		if isEmpty(L) then 
			return;
		end if;
		ptr := L.sentinel.next;
		while ptr /= L.sentinel loop
			if ptr.E = E then
				ptr.prev.next := ptr.next;
				ptr.next.prev := ptr.prev;
				dispose(ptr);
				L.count := L.count - 1;
				if L.count = 0 then
					removeSentinel(L);
				END IF;
				return;
			end if;
			ptr := ptr.next;
		end loop;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.RemoveElement --" & kLFString & Exception_Information (error));
         raise;
	end RemoveElement;

   function MoveFront (L : IN ListType) return listIteratorType is
		I  : listIteratorType;
   BEGIN
      IF L.count = 0 THEN
			I.ptr := null;
			I.sentinel := null;
	   else
			I.ptr := L.Sentinel.Next;
			I.sentinel := L.sentinel;
      end if;
		return I;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.MoveFront --" & kLFString & Exception_Information (error));
         raise;
   END MoveFront;

   function MoveEnd (L : IN ListType) return listIteratorType is
		I  : listIteratorType;
   BEGIN
      IF L.count = 0 THEN
			I.ptr := null;
			I.sentinel := null;
	   else
			I.ptr := L.Sentinel.prev;
			I.sentinel := L.sentinel;
      end if;
		return I;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.MoveEnd --" & kLFString & Exception_Information (error));
         raise;
   END MoveEnd;

   function MoveNext (I : in listIteratorType) return listIteratorType is
		J  : listIteratorType := I;
   BEGIN
		if J.ptr = null or else J.ptr.next = I.sentinel then
			J.ptr := null;
			J.sentinel := null;
	   else
		   J.ptr := J.ptr.next;
	   end if;
		return J;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.MoveNext --" & kLFString & Exception_Information (error));
         raise;
   END MoveNext;

   function MovePrevious (I : in listIteratorType) return listIteratorType is
		J  : listIteratorType := I;
   BEGIN
		if J.ptr = null or else J.ptr.prev = I.sentinel then
			J.ptr := null;
			J.sentinel := null;
	   else
		   J.ptr := J.ptr.prev;
	   end if;
		return J;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.MovePrevious --" & kLFString & Exception_Information (error));
         raise;
   END MovePrevious;

   function getCount (L : in listType) return natural is
   begin
      return L.count;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.getCount --" & kLFString & Exception_Information (error));
         raise;
   end getCount;

   FUNCTION GetCurrent (I : IN listIteratorType) RETURN ElementType is
   BEGIN
      RETURN I.ptr.e;
   exception
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.GetCurrent --" & kLFString & Exception_Information (error));
         raise;
   END GetCurrent;

   FUNCTION GetFront (L : IN ListType) RETURN ElementType IS
   BEGIN
      IF L.count = 0 THEN
         RAISE ListEmpty;
      END IF;
      return L.sentinel.next.e;
   exception
      when listEmpty =>
         raise;
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.GetFront --" & kLFString & Exception_Information (error));
         raise;
   end GetFront;

   FUNCTION GetEnd (L : IN ListType) RETURN ElementType IS
   BEGIN
      IF L.count = 0 THEN
         RAISE ListEmpty;
      END IF;
      return L.sentinel.prev.e;
   exception
      when listEmpty =>
         raise;
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.GetEnd --" & kLFString & Exception_Information (error));
         raise;
   end GetEnd;

   FUNCTION GetNextToFront (L : IN ListType) RETURN ElementType IS
   BEGIN
      IF L.count = 0 THEN
         RAISE ListEmpty;
      END IF;
      if L.count = 1 then
         raise ListContainsOneElement;
      end if;
      return L.sentinel.next.next.e;
   exception
      when listEmpty =>
         raise;
      when listContainsOneElement =>
         raise;
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.GetNextToFront --" & kLFString & Exception_Information (error));
         raise;
   end GetNextToFront;

   FUNCTION GetNextToEnd (L : IN ListType) RETURN ElementType IS
   BEGIN
      IF L.count = 0 THEN
         RAISE ListEmpty;
      END IF;
      if L.count = 1 then
         raise ListContainsOneElement;
      end if;
      return L.sentinel.prev.prev.e;
   exception
      when listEmpty =>
         raise;
      when listContainsOneElement =>
         raise;
	   when error : others =>
		   put_line("UNPLANNED EXCEPTION in ListGeneric.GetNextToEnd --" & kLFString & Exception_Information (error));
         raise;
   end GetNextToEnd;

   -- procedure put (file : in out file_type; L : in out listType) is
   -- begin
     -- moveFront(L);
     -- while hasPosition(L) loop
        -- put(file, getCurrent(L));
        -- moveNext(L);
     -- end loop;
   -- exception
	   -- when error : others =>
		   -- put_line("UNPLANNED EXCEPTION in ListGeneric.put --" & kLFString & Exception_Information (error));
         -- raise;
   -- end put;

END;
