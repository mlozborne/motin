WITH Layout_Tests;

PACKAGE BODY Layout_Suite IS
   FUNCTION Suite RETURN Access_Test_Suite IS
      Ret : CONSTANT Access_Test_Suite :=
      NEW Test_Suite;
   BEGIN
      Ret.Add_Test(NEW Layout_Tests.Test);
      RETURN Ret;
   END Suite;
END Layout_Suite;

