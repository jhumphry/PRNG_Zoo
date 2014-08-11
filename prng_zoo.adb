--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package body PRNG_Zoo is

   --------------
   -- Generate --
   --------------

   function Generate (G: in out PRNG_32Only) return U64 is
      R1 : U32 := Generate(PRNG_32Only'Class(G));
      R2 : U32 := Generate(PRNG_32Only'Class(G));
   begin
      return Shift_Left(U64(R1), 32) or U64(R2);
   end Generate;

end PRNG_Zoo;
