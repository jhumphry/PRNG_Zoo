--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Linear_Congruential;

package PRNG_Zoo.Misc is

   -- Duplicates glibc's random() function, based on the description:
   -- http://www.mscs.dal.ca/~selinger/random/
   -- by Peter Selinger

   type glibc_random is new PRNG_32Only with private;
   function Strength (G : in glibc_random) return PRNG_Strength is (Low);
   procedure Reset (G : in out glibc_random; S : in U64);
   function Generate(G : in out glibc_random) return U32;

   -- Some C++ compatibility
   -- Stephen K. Park and Keith W. Miller (1988).
   -- "Random Number Generators: Good Ones Are Hard To Find".
   -- Communications of the ACM 31 (10): 1192–1201. doi:10.1145/63039.63042.

   package MINSTD_package is
     new Linear_Congruential.Generic_LCG_32Only(Modulus => 2**31 - 1,
                                                Multiplier => 48271,
                                                Increment => 0);
   subtype MINSTD is MINSTD_package.LCG_32Only;

   package MINSTD0_package is
     new Linear_Congruential.Generic_LCG_32Only(Modulus => 2**31 - 1,
                                                Multiplier => 16807,
                                                Increment => 0);
   subtype MINSTD0 is MINSTD0_package.LCG_32Only;

   -- Now a (really terrible) historical curiosity
   package RANDU_package is
     new Linear_Congruential.Generic_LCG_32Only(Modulus => 2**31,
                                                Multiplier => 65539,
                                                Increment => 0);
   subtype RANDU is RANDU_package.LCG_32Only;

private

   type glibc_random_index is mod 34;
   type glibc_random_state is array (glibc_random_index) of U32;

   type glibc_random is new PRNG_32Only with
      record
         s : glibc_random_state;
         p : glibc_random_index := 0;
      end record;

end PRNG_Zoo.Misc;
