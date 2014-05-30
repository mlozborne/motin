WITH AUnit.Reporter.Text;
WITH AUnit.Run;
WITH Layout_Suite;
USE Layout_Suite;

PROCEDURE Test_Layout IS
   PROCEDURE Runner IS NEW AUnit.Run.Test_Runner(Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
BEGIN
   Runner(Reporter);
END Test_Layout;
