--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

with PRNG_Zoo;
use PRNG_Zoo;

package body LFib_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out LFib_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_LFib'Access, "Basic sanity checks on Lagged Fibonacci generator.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : LFib_Test) return Test_String is
   begin
      return Format ("Lagged Fibonacci PRNG Tests");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out LFib_Test) is
   begin
      null;
   end Set_Up;

end LFib_Tests;
