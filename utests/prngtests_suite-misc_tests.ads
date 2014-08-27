--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.Misc;
with PRNGTests_Suite.Sanity_Checks;
with PRNGTests_Suite.Sanity_Checks32;

package PRNGTests_Suite.Misc_Tests is

   type Misc_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Misc_Test);

   function Name (T : Misc_Test) return Test_String;

   procedure Set_Up (T : in out Misc_Test);

   -- Test Routines:
   procedure Test_Glib_Random (T : in out Test_Cases.Test_Case'Class);

   procedure Sanity_Check_GLib_Random is new PRNGTests_Suite.Sanity_Checks32(P => Misc.glibc_random);
   procedure Sanity_Check_KISS is new PRNGTests_Suite.Sanity_Checks32(P => Misc.KISS);
   procedure Sanity_Check_MurmurHash3 is new PRNGTests_Suite.Sanity_Checks(P => Misc.MurmurHash3);

end PRNGTests_Suite.Misc_Tests;
