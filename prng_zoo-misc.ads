--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Misc is

   -- Duplicates glibc's random() function, based on the description:
   -- http://www.mscs.dal.ca/~selinger/random/
   -- by Peter Selinger

   type glibc_random is new PRNG_32Only with private;
   function Strength (G : in glibc_random) return PRNG_Strength is (Low);
   procedure Reset (G : in out glibc_random; S : in U64);
   function Generate(G : in out glibc_random) return U32;

   -- KISS generator - a combination of a multiply-with-carry, a 3-shift
   -- register and a congruential generator
   -- Marsaglia G (1999). “Random Numbers for C: End, at last?”
   -- Posting to sci.stat.math.

   type KISS is new PRNG_32Only with private;
   function Strength (G : in KISS) return PRNG_Strength is (Medium);
   -- S = 0 resets to Marsaglia's suggested starting parameters
   procedure Reset (G : in out KISS; S : in U64);
   function Generate(G : in out KISS) return U32;

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

end PRNG_Zoo.Misc;
