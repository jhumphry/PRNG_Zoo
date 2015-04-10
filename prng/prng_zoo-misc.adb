--
-- PRNG Zoo
-- Copyright (c) 2014 - 2015, James Humphry
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
-- REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
-- INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
-- LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
-- OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
-- PERFORMANCE OF THIS SOFTWARE.

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

   -----------
   -- Reset --
   -----------

   procedure Reset (G : in out KISS; S : in U64) is
      S1 : U32 := U32(S and 16#FFFFFFFF#);
      S2 : U32 := U32(Shift_Right(S, 32));
   begin
      G.z := 362436069 xor S1;
      G.w := 521288629 xor S2;
      G.jsr := 123456789 + S1;
      G.jcong := 380116160 + S2;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G : in out KISS) return U32 is
   begin
      G.z := 36969 * (G.z and 16#FFFF#) + Shift_Right(G.z, 16);
      G.w := 18000 * (G.w and 16#FFFF#) + Shift_Right(G.w, 16);
      G.jsr := G.jsr xor Shift_Left(G.jsr, 17);
      G.jsr := G.jsr xor Shift_Right(G.jsr, 13);
      G.jsr := G.jsr xor Shift_Left(G.jsr, 5);
      G.jcong := 69069 * G.jcong + 1234567;
      return ((Shift_Left(G.z, 16) + G.w) xor G.jcong) + G.jsr;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (G : in out MurmurHash3; S : in U64) is
   begin
      if S = 0 then
         raise Constraint_Error with "MurmurHash3 cannot be seeded with zero.";
      end if;
      G.s := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G : in out MurmurHash3) return U64 is
   begin
      G.s := G.s xor Shift_Right(G.s, 33);
      G.s := G.s * 16#FF51AFD7ED558CCD#;
      G.s := G.s xor Shift_Right(G.s, 33);
      G.s := G.s * 16#C4CEB9FE1A85EC53#;
      G.s := G.s xor Shift_Right(G.s, 33);
      return G.s;
   end Generate;

end PRNG_Zoo.Misc;
