--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.Misc;
use PRNG_Zoo;
use all type PRNG_Zoo.Misc.glibc_random;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure test_glibrandom is
   package U32_IO is new Ada.Text_IO.Modular_IO(Num => U32);
   use U32_IO;
   G_glibc : Misc.glibc_random;
begin

   Put_Line("64 outputs from glibc generator with default seed 1.");
   Reset(G_glibc, 1);
   for I in 0..63 loop
      Put(U32'(Generate(G_glibc)));
      Put(",");
      if I mod 4 = 3 then
         New_Line;
      end if;
   end loop;

end test_glibrandom;
