WITH Interfaces.C;
PACKAGE API IS USE Interfaces;
   FUNCTION AddEm(A : C.Double; B : C.Double) RETURN C.Double;
PRIVATE
   PRAGMA Import(C, AddEm);
END API;

