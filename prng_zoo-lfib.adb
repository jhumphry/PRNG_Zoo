--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package body PRNG_Zoo.LFib is

   ------------------
   -- Generic_LFib --
   ------------------

   package body Generic_LFib is

      -----------
      -- Reset --
      -----------

      procedure Reset (G: in out LFib; S: in U64) is
         Dummy : U64 := 0;
      begin
         for I in G.s'Range loop
            G.s(I) := S xor U64(I);
         end loop;
         G.p := 0;
         for I in 1..k loop
            Dummy := Dummy + G.Generate;
         end loop;
      end Reset;

      --------------
      -- Generate --
      --------------

      function Generate (G: in out LFib) return U64 is
         Result : U64;
      begin
         Result := Op(G.s((G.p-j) mod k), G.s((G.p-k) mod k));
         G.s(G.p) := Result;
         G.p := (G.p + 1) mod k;
         return Result;
      end Generate;

   end Generic_LFib;

end PRNG_Zoo.LFib;
