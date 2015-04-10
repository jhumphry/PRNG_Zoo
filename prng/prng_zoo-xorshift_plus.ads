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

package PRNG_Zoo.xorshift_plus is

   -- Based on (Vigna, 2014b)
   type xorshift128_plus is new PRNG_64Only with private;

   function Strength(G: in xorshift128_plus) return PRNG_Strength is (Medium);

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift128_plus;

   procedure Reset(G: in out xorshift128_plus; S: in U64);

   function Generate(G: in out xorshift128_plus) return U64 with inline;

private

   type xorshift128_plus is new PRNG_64Only with
      record
         s0, s1: U64;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift128_plus is
     (xorshift128_plus'(others => <>));

end PRNG_Zoo.xorshift_plus;
