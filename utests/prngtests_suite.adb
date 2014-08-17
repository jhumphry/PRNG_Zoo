--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Lin_Con_Tests;
with MT_Tests;
with xorshift_star_Tests;
with xorshift_plus_Tests;
with LFib_Tests;

package body PRNGTests_Suite is
   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Test_Lin_Con : aliased Lin_Con_Tests.Lin_Con_Test;
   Test_MT : aliased MT_Tests.MT_Test;
   Test_xorshift_star : aliased xorshift_star_Tests.xorshift_star_Test;
   Test_xorshift_plus : aliased xorshift_plus_Tests.xorshift_plus_Test;
   Test_LFib : aliased LFib_Tests.LFib_Test;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Lin_Con'Access);
      Add_Test (Result'Access, Test_MT'Access);
      Add_Test (Result'Access, Test_xorshift_star'Access);
      Add_Test (Result'Access, Test_xorshift_plus'Access);
      Add_Test (Result'Access, Test_LFib'Access);
      return Result'Access;
   end Suite;

end PRNGTests_Suite;
