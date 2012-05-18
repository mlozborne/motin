with ListGeneric; 
PACKAGE NaturalListTypePkg IS
   -- List of naturals
   package naturalListPkg  is new ListGeneric(ElementType => natural);
   subtype naturalListType is naturalListPkg.ListType;
   procedure put (str : string; L : naturalListType);
 END;