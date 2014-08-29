--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.Stats;

package PRNGTests_Suite.Stats is

   type Stats_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Stats_Test);

   function Name (T : Stats_Test) return Test_String;

   procedure Set_Up (T : in out Stats_Test);

   procedure Check_Z_Score (T : in out Test_Cases.Test_Case'Class);
   procedure Check_Chi2 (T : in out Test_Cases.Test_Case'Class);

end PRNGTests_Suite.Stats;
