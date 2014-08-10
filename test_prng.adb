--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.xorshift_plus;
use PRNG_Zoo, PRNG_Zoo.xorshift_plus;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure test_prng is
   package U64_IO is new Ada.Text_IO.Modular_IO(Num => U64);
   use U64_IO;
   G: xorshift_plus.xorshift128_plus;
begin

   Put_Line("Ten outputs from xorshift128+ with seed 42.");
   Reset(G, 42);
   for I in 1..10 loop
     Put(I, 2);
      Put(":");
      Put(Generate(G));
      New_Line;
   end loop;

   Put_Line("Ten outputs from xorshift128+ with seed 123456.");
   Reset(G, 123456);
   for I in 1..10 loop
     Put(I, 2);
      Put(":");
      Put(Generate(G));
      New_Line;
   end loop;

end test_prng;
