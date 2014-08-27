--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

package body PRNGTests_Suite.xorshift_Family_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out xorshift_Family_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_xss64'Access, "Basic sanity checks on xorshift64* generator.");
      Register_Routine (T, Sanity_Check_xss1024'Access, "Basic sanity checks on xorshift1024* generator.");
      Register_Routine (T, Sanity_Check_xss4096'Access, "Basic sanity checks on xorshift4096* generator.");
      Register_Routine (T, Sanity_Check_xsp128'Access, "Basic sanity checks on xorshift128+ generator.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : xorshift_Family_Test) return Test_String is
   begin
      return Format ("xorshift family of PRNG Tests");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out xorshift_Family_Test) is
   begin
      null;
   end Set_Up;

end PRNGTests_Suite.xorshift_Family_Tests;
