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

package body PRNG_Zoo.xorshift_plus is

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out xorshift128_plus; S: in U64) is
   begin
      -- This is an ad-hoc expedient for now.

      G.s0 := S;
      G.s1 := (not S) xor Shift_Left(S, 17) xor Shift_Right(S, 23);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xorshift128_plus) return U64 is
      t0: constant U64 := G.s1;
      t1: U64 := G.s0;
   begin
      G.s0 := t0;
      t1 := t1 xor Shift_Left(t1, 23);
      G.s1 := (t1 xor t0 xor Shift_Right(t1, 17) xor Shift_Right(t0, 23));
      return G.s1 + t0;
   end Generate;

end PRNG_Zoo.xorshift_plus;
