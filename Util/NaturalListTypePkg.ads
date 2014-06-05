with ListGeneric; 
PACKAGE NaturalListTypePkg IS
   -- List of naturals
	function toString(e : natural) return string;
   package naturalListPkg  is new ListGeneric(ElementType => natural,
	                                           toString    => toString);
   subtype naturalListType is naturalListPkg.ListType;
   procedure put (str : string; L : naturalListType);	
	-- subtype protectNaturalListType is naturalListPkg.ProtectedListType;	
 END;