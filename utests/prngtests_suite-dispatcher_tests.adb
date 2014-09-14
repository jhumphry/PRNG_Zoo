--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

with PRNG_Zoo.xorshift_star;

with PRNG_Zoo.Linear_Congruential.Examples;

with PRNG_Zoo.Filters;

package body PRNGTests_Suite.Dispatcher_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Dispatcher_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Dispatcher'Access, "Basic check on Dispatcher holder type.");
      Register_Routine (T, Test_Dispatcher_32'Access, "Basic check on Dispatcher_32 holder type.");
      Register_Routine (T, Test_Split_32'Access, "Basic check on Split_32 filter.");
      Register_Routine (T, Test_Bit_Reverse'Access, "Test bit reversing filter.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Dispatcher_Test) return Test_String is
   begin
      return Format ("Test Dispatcher holding types and filters");
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

      G.Reset(5489);
      G2.Reset(5489);
      for I in 1..64 loop
         Assert(U64'(G.Generate) = U64'(G2.Generate),
                "xorshift64* wrapped in a Dispatcher produces different 64-bit output than plain xorshift64*.");
      end loop;

      G.Reset(5432);
      G2.Reset(5432);
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

      G.Reset(5489);
      G2.Reset(5489);
      for I in 1..64 loop
         Assert(U32'(G.Generate) = U32'(G2.Generate),
                "MINSTD wrapped in a Dispatcher produces different 64-bit output than plain MINSTD.");
      end loop;

      G.Reset(5432);
      G2.Reset(5432);
      Assert(U32'(G.Generate) = U32'(G2.Generate),
             "Resetting a MINSTD wrapped in a Dispatcher seems different from resetting a plain MINSTD.");

      Assert(G.Strength = G2.Strength,
             "MINSTD wrapped in a Dispatcher has a different strength than plain MINSTD.");

   end Test_Dispatcher_32;

   -------------------
   -- Test_Split_32 --
   -------------------

   procedure Test_Split_32 (T : in out Test_Cases.Test_Case'Class) is
      IG : aliased Filters.Incrementer;
      G : Filters.Split_32(IG => IG'Access);
      Expected_Array : U32_array := (16#33333334#, 16#22222222#,
                                      16#33333335#, 16#22222222#);
   begin
      G.Reset(16#2222222233333333#);
      for E of Expected_Array loop
         Assert(U32'(G.Generate) = E,
                "Split_32 implementation produces unexpected result.");
      end loop;
   end Test_Split_32;

   ----------------------
   -- Test_Bit_Reverse --
   ----------------------

   procedure Test_Bit_Reverse (T : in out Test_Cases.Test_Case'Class) is
      IG : aliased Filters.Incrementer;
      G : Filters.Bit_Reverse(IG => IG'Access);
      Expected_Array_32 : U32_array := (16#2CCCCCCC#, 16#ACCCCCCC#,
                                        16#6CCCCCCC#, 16#ECCCCCCC#);
      Expected_Array_64 : U64_array := (16#2CCCCCCC44444444#,
                                        16#ACCCCCCC44444444#,
                                        16#6CCCCCCC44444444#,
                                        16#ECCCCCCC44444444#);
   begin
      G.Reset(16#33333333#);
      for E of Expected_Array_32 loop
         Assert(U32'(G.Generate) = E,
                "Bit Reverse implementation produces unexpected result in 32 bits.");
      end loop;

      G.Reset(16#2222222233333333#);
      for E of Expected_Array_64 loop
         Assert(U64'(G.Generate) = E,
                "Bit Reverse implementation produces unexpected result in 64 bits.");
      end loop;

   end Test_Bit_Reverse;

end PRNGTests_Suite.Dispatcher_Tests;
