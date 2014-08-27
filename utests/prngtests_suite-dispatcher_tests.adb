--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

with PRNG_Zoo.xorshift_star;
use all type PRNG_Zoo.xorshift_star.xorshift64_star;

with PRNG_Zoo.Linear_Congruential.Examples;
use all type PRNG_Zoo.Linear_Congruential.Examples.MINSTD;

package body PRNGTests_Suite.Dispatcher_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Dispatcher_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Dispatcher'Access, "Basic check on Dispatcher holder type.");
      Register_Routine (T, Test_Dispatcher_32'Access, "Basic check on Dispatcher_32 holder type.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Dispatcher_Test) return Test_String is
   begin
      return Format ("Test Dispatcher holding types");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Dispatcher_Test) is
   begin
      null;
   end Set_Up;

   ---------------------
   -- Test_Dispatcher --
   ---------------------

   procedure Test_Dispatcher (T : in out Test_Cases.Test_Case'Class) is
      G1: aliased xorshift_star.xorshift64_star;
      G2 : xorshift_star.xorshift64_star;
      G : Dispatcher(G1'Access);
   begin

      Reset(G, 5489);
      Reset(G2, 5489);
      for I in 1..64 loop
         Assert(U64'(G.Generate) = U64'(G2.Generate),
                "xorshift64* wrapped in a Dispatcher produces different 64-bit output than plain xorshift64*.");
      end loop;

      Reset(G, 5432);
      Reset(G2, 5432);
      Assert(U64'(G.Generate) = U64'(G2.Generate),
             "Resetting a xorshift64* wrapped in a Dispatcher seems different from resetting a plain xorshift64*.");

      Assert(G.Strength = G2.Strength,
             "xorshift64* wrapped in a Dispatcher has a different strength than plain xorshift64*.");

   end Test_Dispatcher;

   ------------------------
   -- Test_Dispatcher_32 --
   ------------------------

   procedure Test_Dispatcher_32 (T : in out Test_Cases.Test_Case'Class) is
      G1: aliased Linear_Congruential.Examples.MINSTD;
      G2 : Linear_Congruential.Examples.MINSTD;
      G : Dispatcher(G1'Access);
   begin

      Reset(G, 5489);
      Reset(G2, 5489);
      for I in 1..64 loop
         Assert(U32'(G.Generate) = U32'(G2.Generate),
                "MINSTD wrapped in a Dispatcher produces different 64-bit output than plain MINSTD.");
      end loop;

      Reset(G, 5432);
      Reset(G2, 5432);
      Assert(U32'(G.Generate) = U32'(G2.Generate),
             "Resetting a MINSTD wrapped in a Dispatcher seems different from resetting a plain MINSTD.");

      Assert(G.Strength = G2.Strength,
             "MINSTD wrapped in a Dispatcher has a different strength than plain MINSTD.");

   end Test_Dispatcher_32;

end PRNGTests_Suite.Dispatcher_Tests;
