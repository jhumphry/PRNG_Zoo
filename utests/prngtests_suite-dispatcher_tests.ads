--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package PRNGTests_Suite.Dispatcher_Tests is

   type Dispatcher_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Dispatcher_Test);

   function Name (T : Dispatcher_Test) return Test_String;

   procedure Set_Up (T : in out Dispatcher_Test);

   -- Test Routines:
   procedure Test_Dispatcher (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Dispatcher_32 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Split_32 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Bit_Reverse (T : in out Test_Cases.Test_Case'Class);


end PRNGTests_Suite.Dispatcher_Tests;
