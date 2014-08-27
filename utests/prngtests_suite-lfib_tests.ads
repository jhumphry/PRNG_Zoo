--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.LFib;
with PRNGTests_Suite.Sanity_Checks;

package PRNGTests_Suite.LFib_Tests is

   type LFib_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out LFib_Test);

   function Name (T : LFib_Test) return Test_String;

   procedure Set_Up (T : in out LFib_Test);

   package Example_LFib is new LFib.Generic_LFib(j => 24,
                                                          k => 55,
                                                          Op => "+");

   -- Test Routines:
   procedure Sanity_Check_LFib is
     new PRNGTests_Suite.Sanity_Checks(P => Example_LFib.LFib);

end PRNGTests_Suite.LFib_Tests;
