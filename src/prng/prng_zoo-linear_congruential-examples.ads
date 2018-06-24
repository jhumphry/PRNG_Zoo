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
