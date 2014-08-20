--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Linear_Congruential.Examples is

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


end PRNG_Zoo.Linear_Congruential.Examples;
