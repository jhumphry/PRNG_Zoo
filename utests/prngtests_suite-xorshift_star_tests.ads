--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.xorshift_star;
with PRNGTests_Suite.Sanity_Checks;

package PRNGTests_Suite.xorshift_star_Tests is

   type xorshift_star_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out xorshift_star_Test);

   function Name (T : xorshift_star_Test) return Test_String;

   procedure Set_Up (T : in out xorshift_star_Test);

   -- Test Routines:
   procedure Sanity_Check_xss64 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_star.xorshift64_star);
   procedure Sanity_Check_xss1024 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_star.xorshift1024_star);
   procedure Sanity_Check_xss4096 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_star.xorshift4096_star);

end PRNGTests_Suite.xorshift_star_Tests;
