--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Misc is

   -- Duplicates glibc's random() function, based on the description
   -- http://www.mscs.dal.ca/~selinger/random/
   -- (Selinger, 2007)

   type glibc_random is new PRNG_32Only with private;
   function Strength (G : in glibc_random) return PRNG_Strength is (Low);
   procedure Reset (G : in out glibc_random; S : in U64);
   function Generate(G : in out glibc_random) return U32 with inline;

   -- KISS generator - a combination of a multiply-with-carry, a 3-shift
   -- register and a congruential generator
   -- (Marsaglia, 1999)

   type KISS is new PRNG_32Only with private;
   function Strength (G : in KISS) return PRNG_Strength is (Medium);
   -- S = 0 resets to Marsaglia's suggested starting parameters
   procedure Reset (G : in out KISS; S : in U64);
   function Generate(G : in out KISS) return U32 with inline;

   -- MurmurHash3
   -- This is the finalisation stage of MurmurHash3
   -- (https://code.google.com/p/smhasher/wiki/MurmurHash3)
   -- used iteratively as a PRNG as suggested by S Vigna. He comments:

   -- The multipliers are invertible in Z/2^64Z, and the xor/shifts are
   -- invertible in (Z/2Z)^64, so you'll never get zero starting from a
   -- nonzero value. Nonetheless, we have no clue of the period. In
   -- principle, hitting a bad seed you might get into a very short repeating
   -- sequence. The interesting thing is that it passes very well the strongest
   -- statistical tests. It can be useful to scramble user-provided 64-bit
   -- seeds.

   type MurmurHash3 is new PRNG_64Only with private;
   function Strength (G : in MurmurHash3) return PRNG_Strength is (Medium);
   procedure Reset (G : in out MurmurHash3; S : in U64);
   function Generate(G : in out MurmurHash3) return U64 with inline;

private

   type glibc_random_index is mod 34;
   type glibc_random_state is array (glibc_random_index) of U32;

   type glibc_random is new PRNG_32Only with
      record
         s : glibc_random_state;
         p : glibc_random_index := 0;
      end record;

   type KISS is new PRNG_32Only with
      record
         z : U32 := 362436069;
         w : U32 := 521288629;
         jsr : U32 := 123456789;
         jcong : U32 := 380116160;
      end record;

   type MurmurHash3 is new PRNG_64Only with
      record
         s : U64 := 314159263;
      end record;

end PRNG_Zoo.Misc;