--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.xorshift_star is

   type xorshift_star is interface and PRNG;

   type xorshift64_star is new PRNG_64Only and xorshift_star with private;
   function Strength (G : in xorshift64_star) return PRNG_Strength is (Medium);
   procedure Reset (G : in out xorshift64_star; S : in U64);
   function Generate (G : in out xorshift64_star) return U64 with inline;

   type xorshift1024_star is new PRNG_64Only and xorshift_star  with private;
   function Strength (G : in xorshift1024_star) return PRNG_Strength is (High);
   procedure Reset (G : in out xorshift1024_star; S : in U64);
   function Generate (G : in out xorshift1024_star) return U64 with inline;

   type xorshift4096_star is new PRNG_64Only and xorshift_star  with private;
   function Strength (G : in xorshift4096_star) return PRNG_Strength is (High);
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

   type p16 is mod 2**4;
   type p16_arrayU64 is array (p16) of U64;
   type xorshift1024_star is new PRNG_64Only and xorshift_star with record
      s : p16_arrayU64;
      p : p16 := 0;
   end record;

   type p64 is mod 2**6;
   type p64_arrayU64 is array (p64) of U64;
   type xorshift4096_star is new PRNG_64Only and xorshift_star with record
      s : p64_arrayU64;
      p : p64 := 0;
   end record;

end PRNG_Zoo.xorshift_star;
