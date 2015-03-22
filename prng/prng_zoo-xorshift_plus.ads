--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

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
