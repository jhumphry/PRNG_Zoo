--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Interfaces;
use all type Interfaces.Integer_64;

package body PRNG_Zoo.Misc is

   -----------
   -- Reset --
   -----------

   procedure Reset (G : in out glibc_random; S : in U64) is
      M : constant Interfaces.Integer_64 := 2**31-1;
      C : constant := 16807;
      r : Interfaces.Integer_64;
      dummy : U32 := 0;
   begin
      G.s(0) := U32(S mod 2**32);
      for I in glibc_random_index range 1..30 loop
         r := Interfaces.Integer_64(G.s(I-1));
         G.s(I) := U32( ((r * C) mod M) mod 2**32);
      end loop;
      for I in glibc_random_index range 31..33 loop
         G.s(I) := G.s(I-31);
      end loop;
      G.p := 0;
      for I in 34..343 loop
         dummy := dummy + Generate(G);
      end loop;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G : in out glibc_random) return U32 is
   begin
      G.s(G.p) := G.s(G.p-3) + G.s(G.p-31);
      G.p := G.p +1;
      return Shift_Right(G.s(G.p-1),1);
   end Generate;

end PRNG_Zoo.Misc;
