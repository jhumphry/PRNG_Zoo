--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.Misc;

with Sanity_Checks32;

package Lin_Con_Tests is

   type Lin_Con_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Lin_Con_Test);

   function Name (T : Lin_Con_Test) return Test_String;

   procedure Set_Up (T : in out Lin_Con_Test);

   -- Test Routines:
   procedure Sanity_Check_MINSTD is new Sanity_Checks32(P => PRNG_Zoo.Misc.MINSTD);
   procedure Sanity_Check_MINSTD0 is new Sanity_Checks32(P => PRNG_Zoo.Misc.MINSTD0);
   procedure Sanity_Check_RANDU is new Sanity_Checks32(P => PRNG_Zoo.Misc.RANDU);

   procedure Test_RANDU (T : in out Test_Cases.Test_Case'Class);

   procedure Test_MINSTD (T : in out Test_Cases.Test_Case'Class);

end Lin_Con_Tests;
