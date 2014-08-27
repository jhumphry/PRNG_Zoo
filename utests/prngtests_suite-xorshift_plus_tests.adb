--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

package body PRNGTests_Suite.xorshift_plus_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out xorshift_plus_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_xsp128'Access, "Basic sanity checks on xorshift128+ generator.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : xorshift_plus_Test) return Test_String is
   begin
      return Format ("xorshift+ PRNG Tests");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out xorshift_plus_Test) is
   begin
      null;
   end Set_Up;

end PRNGTests_Suite.xorshift_plus_Tests;
