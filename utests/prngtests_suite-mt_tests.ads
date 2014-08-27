--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.MT;
with PRNGTests_Suite.Sanity_Checks;
with PRNGTests_Suite.Sanity_Checks32;

package PRNGTests_Suite.MT_Tests is

   type MT_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out MT_Test);

   function Name (T : MT_Test) return Test_String;

   procedure Set_Up (T : in out MT_Test);

   -- Test Routines:
   procedure Test_MT19937 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_MT19937_64 (T : in out Test_Cases.Test_Case'Class);

   procedure Sanity_Check_MT19937 is new PRNGTests_Suite.Sanity_Checks32(P => MT.MT19937);
   procedure Sanity_Check_MT19937_64 is new PRNGTests_Suite.Sanity_Checks(P => MT.MT19937_64);


end PRNGTests_Suite.MT_Tests ;
