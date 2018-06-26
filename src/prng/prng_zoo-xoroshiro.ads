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

   type xoroshiro is abstract new PRNG_64Only with private;
   procedure Reset(G: in out xoroshiro; S: in U64);

   -- Suggested for a 64 bit generator in Blackman and Vigna 2017
   -- using parameters (a=24, b=16, c=37) in preference to the parameters
   -- (a=55, b=14, c=37) used in an early version proposed by B&V.
   type xoroshiro128_plus is new xoroshiro with private;
   function Strength(G: in xoroshiro128_plus) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoroshiro128_plus;
   function Generate(G: in out xoroshiro128_plus) return U64 with inline;

   -- Suggested for a 64 bit generator in Blackman and Vigna 2017
   type xoroshiro128_star_star is new xoroshiro with private;
   function Strength(G: in xoroshiro128_star_star) return PRNG_Strength is (High);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoroshiro128_star_star;
   function Generate(G: in out xoroshiro128_star_star) return U64 with inline;

private

   type xoroshiro is abstract new PRNG_64Only with
      record
         s0, s1 : U64 ;
      end record;

   type xoroshiro128_plus is new xoroshiro with null record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoroshiro128_plus is
     (xoroshiro128_plus'(xoroshiro with others => <>));

   type xoroshiro128_star_star is new xoroshiro with null record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoroshiro128_star_star is
     (xoroshiro128_star_star'(xoroshiro with others => <>));

end PRNG_Zoo.xoroshiro;
