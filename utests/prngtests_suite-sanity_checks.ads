--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Test_Cases; use AUnit.Test_Cases;

generic
   type P is new PRNG with private;
   N : Positive := 64;
   seed1 : U64 := 27182818;
   seed2 : U64 := 31415926;
procedure PRNGTests_Suite.Sanity_Checks(T : in out Test_Case'Class);
