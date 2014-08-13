--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.Misc, PRNG_Zoo.Linear_Congruential;
use PRNG_Zoo;
use all type PRNG_Zoo.Misc.RANDU;
use all type PRNG_Zoo.Misc.MINSTD;
use all type PRNG_Zoo.Misc.MINSTD0;
use all type PRNG_Zoo.Linear_Congruential.LCG_32Only;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure test_misc is
   package U32_IO is new Ada.Text_IO.Modular_IO(Num => U32);
   use U32_IO;
   G_RANDU : Misc.RANDU;
   G_MINSTD : Misc.MINSTD;
   G_MINSTD0 : Misc.MINSTD0;
   G_MINSTD0_Copy : Linear_Congruential.LCG_32Only(Modulus => 2147483647,
                                                   Multiplier => 16807,
                                                   Increment => 0);
begin

   Put_Line("Ten outputs from RANDU generator with default seed 1.");
   Reset(G_RANDU, 1);
   for I in 0..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(Generate(G_RANDU)));
      New_Line;
   end loop;

   Put_Line("Ten outputs from MINSTD generator with default seed 1.");
   Reset(G_MINSTD, 1);
   for I in 0..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(Generate(G_MINSTD)));
      New_Line;
   end loop;

   Put_Line("Ten outputs from MINSTD0 generator with default seed 1.");
   Reset(G_MINSTD0, 1);
   for I in 0..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(Generate(G_MINSTD0)));
      New_Line;
   end loop;

   Put_Line("Ten outputs from parametizable linear congruence generator with MINSTD0 parameters and default seed 1.");
   Reset(G_MINSTD0_Copy, 1);
   for I in 0..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(Generate(G_MINSTD0_Copy)));
      New_Line;
   end loop;

end test_misc;
