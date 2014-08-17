--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.LFib is

   generic
      j,k : Positive;
      with function Op(Left, Right: U64) return U64;
   package Generic_LFib is
      type LFib is new PRNG with private;
      function Strength(G: in LFib) return PRNG_Strength is (Low);
      procedure Reset(G: in out LFib; S: in U64);
      function Generate(G: in out LFib) return U64;

   private
      type LFib is new PRNG with
         record
            s : U64_array(0..k-1);
            p : Integer := 0;
         end record;
   end Generic_LFib;

end PRNG_Zoo.LFib;
