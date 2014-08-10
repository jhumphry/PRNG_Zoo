--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.xorshift_plus, PRNG_Zoo.xorshift_star;
use PRNG_Zoo;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure test_prng is
   package U64_IO is new Ada.Text_IO.Modular_IO(Num => U64);
   use U64_IO;
   G_x128p : xorshift_plus.xorshift128_plus;
   G_x64s: xorshift_star.xorshift64_star;
   G_x1024s: xorshift_star.xorshift1024_star;
   G_x4096s: xorshift_star.xorshift4096_star;
begin

   Put_Line("Ten outputs from xorshift128+ with seed 123456.");
   xorshift_plus.Reset(G_x128p, 123456);
   for I in 1..10 loop
     Put(I, 2);
      Put(":");
      Put(xorshift_plus.Generate(G_x128p));
      New_Line;
   end loop;

   Put_Line("Ten outputs from xorshift64* with seed 123456.");
   xorshift_star.Reset(G_x64s, 123456);
   for I in 1..10 loop
     Put(I, 2);
      Put(":");
      Put(xorshift_star.Generate(G_x64s));
      New_Line;
   end loop;

   Put_Line("Ten outputs from xorshift1024* with seed 123456.");
   xorshift_star.Reset(G_x1024s, 123456);
   for I in 1..10 loop
     Put(I, 2);
      Put(":");
      Put(xorshift_star.Generate(G_x1024s));
      New_Line;
   end loop;

   Put_Line("Ten outputs from xorshift4096* with seed 123456.");
   xorshift_star.Reset(G_x4096s, 123456);
   for I in 1..10 loop
     Put(I, 2);
      Put(":");
      Put(xorshift_star.Generate(G_x4096s));
      New_Line;
   end loop;

end test_prng;
