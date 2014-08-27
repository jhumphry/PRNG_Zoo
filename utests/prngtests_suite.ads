--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;
use all type PRNG_Zoo.U32;
use all type PRNG_Zoo.U64;

with AUnit.Test_Suites;

package PRNGTests_Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end PRNGTests_Suite;
