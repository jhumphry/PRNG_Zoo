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

package PRNG_Zoo.xoroshiro is

   type xoroshiro is interface and PRNG;

   -- Suggested for a 64 bit generator in Blackman and Vigna 2017
   type xoroshiro128_plus is new PRNG_64Only and xoroshiro with private;
   -- using parameters (a=24, b=16, b=37) in preference to the parameters
   -- (a=55, b=14, c=37) used in an early version proposed by B&V.
   function Strength(G: in xoroshiro128_plus) return PRNG_Strength is (High);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoroshiro128_plus;
   procedure Reset(G: in out xoroshiro128_plus; S: in U64);
   function Generate(G: in out xoroshiro128_plus) return U64 with inline;

private

   type xoroshiro128_plus is new PRNG_64Only and xoroshiro with
      record
         s0, s1 : U64 ;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoroshiro128_plus is
     (xoroshiro128_plus'(others => <>));

end PRNG_Zoo.xoroshiro;
