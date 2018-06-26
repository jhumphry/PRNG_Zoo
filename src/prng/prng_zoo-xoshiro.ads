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

package PRNG_Zoo.xoshiro is

   type xoshiro256 is abstract new PRNG_64Only with private;
   procedure Reset(G: in out xoshiro256; S: in U64);

   -- Suggested for a 64 bit generator in Blackman and Vigna 2018
   -- using parameters (a=17, b=45, c=-)
   type xoshiro256_plus is new xoshiro256 with private;
   function Strength(G: in xoshiro256_plus) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoshiro256_plus;
   function Generate(G: in out xoshiro256_plus) return U64 with inline;

   -- Suggested for a 64 bit generator in Blackman and Vigna 2018
   -- using parameters (a=17, b=45, c=-)
   type xoshiro256_star_star is new xoshiro256 with private;
   function Strength(G: in xoshiro256_star_star) return PRNG_Strength is (High);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoshiro256_star_star;
   function Generate(G: in out xoshiro256_star_star) return U64 with inline;

private

   type xoshiro256 is abstract new PRNG_64Only with
      record
         s0, s1, s2, s3 : U64 ;
      end record;

   type xoshiro256_plus is new xoshiro256 with null record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoshiro256_plus is
     (xoshiro256_plus'(xoshiro256 with others => <>));

   type xoshiro256_star_star is new xoshiro256 with null record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xoshiro256_star_star is
     (xoshiro256_star_star'(xoshiro256 with others => <>));

end PRNG_Zoo.xoshiro;
