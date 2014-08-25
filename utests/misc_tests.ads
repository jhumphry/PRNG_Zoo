--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.Misc;
with Sanity_Checks;
with Sanity_Checks32;

package Misc_Tests is

   type Misc_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Misc_Test);

   function Name (T : Misc_Test) return Test_String;

   procedure Set_Up (T : in out Misc_Test);

   -- Test Routines:
   procedure Test_Glib_Random (T : in out Test_Cases.Test_Case'Class);

   procedure Sanity_Check_GLib_Random is new Sanity_Checks32(P => PRNG_Zoo.Misc.glibc_random);
   procedure Sanity_Check_KISS is new Sanity_Checks32(P => PRNG_Zoo.Misc.KISS);
   procedure Sanity_Check_MurmurHash3 is new Sanity_Checks(P => PRNG_Zoo.Misc.MurmurHash3);

end Misc_Tests;
