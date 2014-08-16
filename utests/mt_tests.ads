--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package MT_Tests is

   type MT_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out MT_Test);

   function Name (T : MT_Test) return Test_String;

   procedure Set_Up (T : in out MT_Test);

   -- Test Routines:
   procedure Test_MT19937 (T : in out Test_Cases.Test_Case'Class);

end MT_Tests;
