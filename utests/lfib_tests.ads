--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Interfaces;
use type Interfaces.Unsigned_64;

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.LFib;
with Sanity_Checks;

package LFib_Tests is

   type LFib_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out LFib_Test);

   function Name (T : LFib_Test) return Test_String;

   procedure Set_Up (T : in out LFib_Test);

   package Example_LFib is new PRNG_Zoo.LFib.Generic_LFib(j => 24,
                                                          k => 55,
                                                          Op => "+");

   -- Test Routines:
   procedure Sanity_Check_LFib is
     new Sanity_Checks(P => Example_LFib.LFib);

end LFib_Tests;
