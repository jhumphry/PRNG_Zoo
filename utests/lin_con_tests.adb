--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

with PRNG_Zoo;
use PRNG_Zoo;
use all type PRNG_Zoo.U32;

with PRNG_Zoo.Linear_Congruential;
with PRNG_Zoo.Linear_Congruential.Examples;
use all type PRNG_Zoo.Linear_Congruential.Examples.RANDU;
use all type PRNG_Zoo.Linear_Congruential.Examples.MINSTD;
use all type PRNG_Zoo.Linear_Congruential.Examples.MINSTD0;
use all type PRNG_Zoo.Linear_Congruential.LCG_32Only;

package body Lin_Con_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Lin_Con_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_MINSTD'Access, "Basic sanity checks on MINSTD generator.");
      Register_Routine (T, Sanity_Check_MINSTD0'Access, "Basic sanity checks on MINSTD0 generator.");
      Register_Routine (T, Sanity_Check_RANDU'Access, "Basic sanity checks on RANDU generator.");
      Register_Routine (T, Test_RANDU'Access, "Test RANDU generator against expected (initial) output");
      Register_Routine (T, Test_MINSTD'Access,
                        "Test generic versions of MINSTD and MINSTD0 generators versus parametised version");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Lin_Con_Test) return Test_String is
   begin
      return Format ("Linear Congruential PRNG Tests");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Lin_Con_Test) is
   begin
      null;
   end Set_Up;

   ----------------
   -- Test_RANDU --
   ----------------

   procedure Test_RANDU (T : in out Test_Cases.Test_Case'Class) is
      G : Linear_Congruential.Examples.RANDU;
      Expected : U32_Array := (65539, 393225, 1769499, 7077969,
                               26542323, 95552217, 334432395, 1146624417,
                               1722371299, 14608041);
   begin
      Reset(G, 1);
      for E of Expected loop
         Assert(U32'(Generate(G)) = E, "RANDU implementation produces unexpected result");
      end loop;
   end Test_RANDU;

   -----------------
   -- Test_MINSTD --
   -----------------

   procedure Test_MINSTD (T : in out Test_Cases.Test_Case'Class) is
      G_MINSTD : Linear_Congruential.Examples.MINSTD;
      G_MINSTD_P : Linear_Congruential.LCG_32Only(Modulus => 2147483647,
                                                  Multiplier => 48271,
                                                  Increment => 0);
      G_MINSTD0 : Linear_Congruential.Examples.MINSTD0;
      G_MINSTD0_P : Linear_Congruential.LCG_32Only(Modulus => 2147483647,
                                                   Multiplier => 16807,
                                                   Increment => 0);

   begin
      Reset(G_MINSTD, 1);
      Reset(G_MINSTD_P, 1);
      Reset(G_MINSTD0, 1);
      Reset(G_MINSTD0_P, 1);

      for I In 1..1024 loop
         Assert(U32'(Generate(G_MINSTD)) = U32'(Generate(G_MINSTD_P)),
                "MINSTD outputs from generic LCG don't match that from parametised LCG.");

         Assert(U32'(Generate(G_MINSTD0)) = U32'(Generate(G_MINSTD0_P)),
                "MINSTD0 outputs from generic LCG don't match that from parametised LCG.");
      end loop;
   end Test_MINSTD;


end Lin_Con_Tests;
