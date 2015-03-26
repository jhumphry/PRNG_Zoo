--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Linear_Congruential.Examples is

   -- Some C++ compatibility
   -- As suggested in (Stephen K. Park and Keith W. Miller 1988).

   package MINSTD_package is
     new Linear_Congruential.Generic_LCG_32Only(Modulus => 2**31 - 1,
                                                Multiplier => 48271,
                                                Increment => 0,
                                                Usable_Width => 31);
   subtype MINSTD is MINSTD_package.LCG_32Only;

   MINSTD_Parameters : aliased LCG_32Only_Parameters := (Modulus => 2**31 - 1,
                                                         Multiplier => 48271,
                                                         Increment => 0,
                                                         Usable_Width => 31,
                                                         s => 1);

   package MINSTD0_package is
     new Linear_Congruential.Generic_LCG_32Only(Modulus => 2**31 - 1,
                                                Multiplier => 16807,
                                                Increment => 0,
                                                Usable_Width => 31);
   subtype MINSTD0 is MINSTD0_package.LCG_32Only;

   MINSTD0_Parameters : aliased LCG_32Only_Parameters := (Modulus => 2**31 - 1,
                                                          Multiplier => 16807,
                                                          Increment => 0,
                                                          Usable_Width => 31,
                                                          s => 1);

   -- Now a (really terrible) historical curiosity
   -- via Wikipedia
   package RANDU_package is
     new Linear_Congruential.Generic_LCG_32Only(Modulus => 2**31,
                                                Multiplier => 65539,
                                                Increment => 0,
                                                Usable_Width => 31);
   subtype RANDU is RANDU_package.LCG_32Only;

   RANDU_Parameters : aliased LCG_32Only_Parameters := (Modulus => 2**31,
                                                Multiplier => 65539,
                                                Increment => 0,
                                                Usable_Width => 31,
                                                s => 1);

end PRNG_Zoo.Linear_Congruential.Examples;
