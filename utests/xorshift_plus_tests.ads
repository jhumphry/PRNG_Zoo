--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.xorshift_plus;
with Sanity_Checks;

package xorshift_plus_Tests is

   type xorshift_plus_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out xorshift_plus_Test);

   function Name (T : xorshift_plus_Test) return Test_String;

   procedure Set_Up (T : in out xorshift_plus_Test);

   -- Test Routines:
   procedure Sanity_Check_xsp128 is
     new Sanity_Checks(P => PRNG_Zoo.xorshift_plus.xorshift128_plus);

end xorshift_plus_Tests;
