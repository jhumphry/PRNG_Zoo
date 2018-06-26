--
-- PRNG Zoo
-- Copyright (c) 2014 - 2018, James Humphry
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

package body PRNG_Zoo.xoshiro is

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out xoshiro256; S: in U64) is
   begin
      -- This is an ad-hoc expedient for now.

      G.s0 := S;
      G.s1 := (not S) xor Shift_Left(S, 17) xor Shift_Right(S, 23);
      G.s2 := S + (S xor Shift_Left(S, 12));
      G.s3 := S + (S xor Shift_Right(S, 27));
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xoshiro256_plus) return U64 is
      result : constant U64 := G.s0 + G.s3;
      t: constant U64 := Shift_Left(G.s1, 17);
   begin
      G.s2 := G.s2 xor G.s0;
      G.s3 := G.s3 xor G.s1;
      G.s1 := G.s1 xor G.s2;
      G.s0 := G.s0 xor G.s3;

      G.s2 := G.s2 xor t;

      G.s3 := Rotate_Left(G.s3, 45);

      return result;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xoshiro256_star_star) return U64 is
      result : constant U64 := Rotate_Left(G.s1 * 5, 7) * 9;
      t: constant U64 := Shift_Left(G.s1, 17);
   begin
      G.s2 := G.s2 xor G.s0;
      G.s3 := G.s3 xor G.s1;
      G.s1 := G.s1 xor G.s2;
      G.s0 := G.s0 xor G.s3;

      G.s2 := G.s2 xor t;

      G.s3 := Rotate_Left(G.s3, 45);

      return result;
   end Generate;

end PRNG_Zoo.xoshiro;
