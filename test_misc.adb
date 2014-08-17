--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Interfaces;
use type Interfaces.Unsigned_64;

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.Misc, PRNG_Zoo.Linear_Congruential;
with PRNG_Zoo.LFib;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure test_misc is
   package U32_IO is new Ada.Text_IO.Modular_IO(Num => U32);
   use U32_IO;
   package U64_IO is new Ada.Text_IO.Modular_IO(Num => U64);
   use U64_IO;

   G_RANDU : Misc.RANDU;
   G_MINSTD : Misc.MINSTD;
   G_MINSTD0 : Misc.MINSTD0;
   G_MINSTD0_Copy : Linear_Congruential.LCG_32Only(Modulus => 2147483647,
                                                   Multiplier => 16807,
                                                   Increment => 0);
   package Example_LFib is new LFib.Generic_LFib(j => 7, k => 10, Op => "+");
   G_LFib : Example_LFib.LFib;

begin

   Put_Line("Ten outputs from RANDU generator with default seed 1.");
   G_RANDU.Reset(1);
   for I in 1..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(G_RANDU.Generate));
      New_Line;
   end loop;

   Put_Line("Ten outputs from MINSTD generator with default seed 1.");
   G_MINSTD.Reset(1);
   for I in 1..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(G_MINSTD.Generate));
      New_Line;
   end loop;

   Put_Line("Ten outputs from MINSTD0 generator with default seed 1.");
   G_MINSTD0.Reset(1);
   for I in 1..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(G_MINSTD0.Generate));
      New_Line;
   end loop;

   Put_Line("Ten outputs from parametizable linear congruence generator with MINSTD0 parameters and default seed 1.");
   G_MINSTD0_Copy.Reset(1);
   for I in 1..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(G_MINSTD0_Copy.Generate));
      New_Line;
   end loop;

   Put_Line("Eleven outputs from Lagged Fibonacci generator j=7, k=10, seed 55776644.");
   G_LFib.Reset(55776644);
   for I in 1..11 loop
      Put(I, 2);
      Put(":");
      Put(G_LFib.Generate);
      New_Line;
   end loop;

end test_misc;
