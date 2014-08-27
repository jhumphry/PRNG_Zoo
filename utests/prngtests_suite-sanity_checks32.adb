--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

procedure PRNGTests_Suite.Sanity_Checks32(T : in out Test_Case'Class) is
   G : P;
   Output : U32_array(1..N);
begin

   Reset(G, seed1);
   for I in Output'Range loop
      Output(I) := Generate(G);
   end loop;
   Assert((for all I in 2..Output'Last => Output(I) /= Output(1)),
          "Generator might be a constant or have very short period.");

   Reset(G, seed1);
   Assert((for all I in Output'Range => Output(I) = Generate(G)),
          "Resetting generator does not restart the same sequence.");

   Reset(G, seed2);
   Assert((for some I in Output'Range => Output(I) /= Generate(G)),
          "Different seeds appear to restart the same sequence.");
end PRNGTests_Suite.Sanity_Checks32;
