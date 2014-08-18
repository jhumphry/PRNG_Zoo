--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.MT;
use PRNG_Zoo;
use all type PRNG_Zoo.MT.MT19937;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure test_mt is
   package U32_IO is new Ada.Text_IO.Modular_IO(Num => U32);
   use U32_IO;
   G_MT19937 : MT.MT19937;
begin

   Put_Line("Ten outputs from MT19937 generator with seed 5489.");
   Reset(G_MT19937, 5489);
   for I in 1..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(Generate(G_MT19937)));
      New_Line;
   end loop;

   Put_Line("Ten outputs from MT19937 generator with seed {0x123, 0x234, 0x345, 0x456}.");
   Reset(G_MT19937, U64_array'(16#0123#, 16#0234#, 16#0345#, 16#0456#));
   for I in 1..10 loop
      Put(I, 2);
      Put(":");
      Put(U32'(Generate(G_MT19937)));
      New_Line;
   end loop;

end test_mt;
