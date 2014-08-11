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

private

   type glibc_random_index is mod 34;
   type glibc_random_state is array (glibc_random_index) of U32;

   type glibc_random is new PRNG_32Only with
      record
         s : glibc_random_state;
         p : glibc_random_index := 0;
      end record;

end PRNG_Zoo.Misc;
