-- A generic list with iterator
-- with ada.text_io; use ada.text_io;

GENERIC

   TYPE ElementType IS PRIVATE;
   WITH function toString (E : ElementType) return string;

PACKAGE ListGeneric IS

   TYPE ListType         IS PRIVATE;
	type listIteratorType is private;

	protected type ProtectedListType is
	   -- No exception thrown
		function toString return string;
		procedure makeEmpty;
		function isEmpty return boolean;
		procedure addFront(e : elementType);
		procedure addEnd(e : elementType);
		procedure removeFront;
		procedure removeEnd;
		procedure removeElement(e : elementType);
		function getCount return natural;
		procedure getFront(e : out elementType; found : out boolean);
		procedure getEnd(e : out elementType; found : out boolean);
	private
		list : ListType;
	end ProtectedListType;

   ListEmpty              : EXCEPTION;
   NoCurrentPosition      : EXCEPTION;
   ListContainsOneElement : exception;

	function toString(L : listType)	return string;
	-- pre:   L is defined.
	-- post:  L is a string representation of the list

   PROCEDURE MakeEmpty (L : IN OUT ListType);
   -- Pre:    L is defined.
   -- Post:   L is empty.
   --         Dynamic memory recovered.

   procedure copyFromTo(source : in listType; dest : in out listType);
   -- Pre:   source and dest are defined
   -- Post:  dest now is a copy of source
   --        Dynamic memory recovered from old dest.

   FUNCTION IsEmpty (L : ListType) RETURN Boolean;
   -- Pre:    L is defined.
   -- Post:   Returns true if L empty else false.

   procedure ReverseList (L : in out ListType);
   -- Pre:    L is defined.
   -- Post:   The elements in L are now in reverse order.
   --         There are no memory leaks.

   FUNCTION IsNull (I : listIteratorType) RETURN Boolean;
   -- Pre:    I is defined.
   -- Post:   Returns false if I is "pointing" at a list node else returns true.

   PROCEDURE AddFront (L : IN OUT ListType; E : IN ElementType);
   -- Pre:    L and E are defined.
   -- Post:   E is inserted at front of list.

   PROCEDURE AddEnd (L : IN OUT ListType; E : IN ElementType);
   -- Pre:    L and E are defined.
   -- Post:   E is inserted at end of list.

   PROCEDURE RemoveFront (L : IN OUT ListType);
   -- Pre:    L is defined.
   -- Post:   First element is removed from list.
   --         Dynamic memory recovered.
   -- Raises: ListEmpty if try to remove from an empty list.

   PROCEDURE RemoveEnd (L : IN OUT ListType);
   -- Pre:    L is defined.
   -- Post:   Last element is removed from list.
   -- Raises: ListEmpty if try to remove from an empty list.

	procedure RemoveElement(L : in out ListType; E : in ElementType);
	-- Pre:    L is defined.
	-- Post:   First node containing E is removed from list, if there is one.

   function inList(L : in listType; E : in ElementType) return boolean;
   -- Pre:    L is defined.
   -- Post:   Returns true if E is in L else returns false

   function MoveFront (L : IN ListType) return listIteratorType;
   -- Pre:    L is defined.
   -- Ret:    If list not empty then returns "pointer" to first node else returns "null".

   function MoveEnd (L : IN ListType) return listIteratorType;
   -- Pre:    L is defined.
   -- Ret:    If list not empty then returns "pointer" to last node else returns "null".

   function MoveNext (I : in listIteratorType) return listIteratorType;
   -- Pre:    I "points" at a list node.
	-- Ret:    If I doesn't "point" at a list node then probably throws a memory access violation.
   --         If I is "null" then returns "null".
	--         If I "points" at last node then returns "null".
	--         Else returns "pointer" to next node.

   function MovePrevious (I : in listIteratorType) return listIteratorType;
   -- Pre:    I "points" at a list node.
	-- Ret:    If I doesn't "point" at a list node, then probably throws a memory access violation.
   --         If I is "null" then returns "null".
	--         If I "points" at first node then returns "null".
	--         Else returns "pointer" to previous node.

   function getCount (L : in listType) return natural;
   -- Pre:    L is defined.
   -- Post:   Returns the number of elements in the list

   FUNCTION GetCurrent (I : IN listIteratorType) RETURN ElementType;
   -- Pre:    I "points" at a list node.
	-- Ret:    If I doesn't "point" at a list node then probably throws a memory access violation.
	--         Else returns value of element at "position" I.

   FUNCTION GetFront (L : IN ListType) RETURN ElementType;
   -- Pre:    L is defined.
   -- Post:   Returns value of element at front.
   -- Raises: ListEmpty if list empty.

   FUNCTION GetNextToFront (L : IN ListType) RETURN ElementType;
   -- Pre:    L is defined.
   -- Post:   Returns value of element next to front.
   -- Raises: ListEmpty and ListContainsOneElement.

   FUNCTION GetEnd (L : IN ListType) RETURN ElementType;
   -- Pre:    L is defined.
   -- Post:   Returns value of element at end.
   -- Raises: ListEmpty if list empty.

   FUNCTION GetNextToEnd (L : IN ListType) RETURN ElementType;
   -- Pre:    L is defined.
   -- Post:   Returns value of element next to end.
   -- Raises: ListEmpty and ListContainsOneElement
   
   -- procedure put (file : in out file_type; L : in out listType);
   -- Pre:    L is defined. File is open for writing
   -- Post:   The elements in the list are displayed in File

PRIVATE

   -- Implemented using doubly linked list with sentinel node.
   -- An empty list has no sentinel node and must be initialized before adding elements.

   kLFString        : string(1..1) := ( 1=> standard.ascii.LF);

   TYPE NodeType;
   TYPE NodePtrType      IS ACCESS NodeType;

   TYPE NodeType IS RECORD
      E    : ElementType;
      Prev : NodePtrType;
      Next : NodePtrType;
   END RECORD;

   TYPE ListType IS RECORD
      Sentinel : NodePtrType := NULL;     -- when list empty
      Count    : Natural     := 0;        -- when list empty
   END RECORD;

   TYPE listIteratorType IS record
		ptr	    : nodePtrType := null;
		sentinel  : nodePtrType := null;
	end record;

END ListGeneric;
