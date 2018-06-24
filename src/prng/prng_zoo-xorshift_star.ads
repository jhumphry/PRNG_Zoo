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

package PRNG_Zoo.xorshift_star is

   -- A family of PRNG based on (Vigna, 2014a)
   type xorshift_star is interface and PRNG;

   type xorshift64_star is new PRNG_64Only and xorshift_star with private;
   function Strength (G : in xorshift64_star) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift64_star;
   procedure Reset (G : in out xorshift64_star; S : in U64);
   function Generate (G : in out xorshift64_star) return U64 with inline;

   type xorshift1024_star is new PRNG_64Only and xorshift_star  with private;
   function Strength (G : in xorshift1024_star) return PRNG_Strength is (High);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift1024_star;
   procedure Reset (G : in out xorshift1024_star; S : in U64);
   function Generate (G : in out xorshift1024_star) return U64 with inline;

   type xorshift4096_star is new PRNG_64Only and xorshift_star  with private;
   function Strength (G : in xorshift4096_star) return PRNG_Strength is (High);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift4096_star;
   procedure Reset (G : in out xorshift4096_star; S : in U64);
   function Generate (G : in out xorshift4096_star) return U64 with inline;

   -- Constants from Table IV in
   -- An experimental exploration of Marsaglia's xorshift generators, scrambled
   -- Sebastiano Vigna
   -- Accessed on arXiv 1402.6246v2

   M32 : constant := 2685821657736338717;
   M8  : constant := 1181783497276652981;
   M2  : constant := 8372773778140471301;

private

   type xorshift64_star is new PRNG_64Only and xorshift_star with record
      s : U64;
   end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return xorshift64_star is
     (xorshift64_star'(others => <>));

   type p16 is mod 2**4;
   type p16_arrayU64 is array (p16) of U64;
   type xorshift1024_star is new PRNG_64Only and xorshift_star with record
      s : p16_arrayU64;
      p : p16 := 0;
   end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return xorshift1024_star is
     (xorshift1024_star'(others => <>));

   type p64 is mod 2**6;
   type p64_arrayU64 is array (p64) of U64;
   type xorshift4096_star is new PRNG_64Only and xorshift_star with record
      s : p64_arrayU64;
      p : p64 := 0;
   end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return xorshift4096_star is
     (xorshift4096_star'(others => <>));

end PRNG_Zoo.xorshift_star;
