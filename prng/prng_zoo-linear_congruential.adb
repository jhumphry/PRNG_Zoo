package body PRNG_Zoo.Linear_Congruential is

   -----------------
   -- Generic_LCG --
   -----------------

   package body Generic_LCG is

      -----------
      -- Reset --
      -----------

      procedure Reset (G: in out LCG; S: in U64) is
      begin
         G.s := S;
      end Reset;

      --------------
      -- Generate --
      --------------

      function Generate (G: in out LCG) return U64 is
      begin
         G.s := (G.s * Multiplier + Increment) mod Modulus;
         return G.s;
      end Generate;

   end Generic_LCG;

   ------------------------
   -- Generic_LCG_32Only --
   ------------------------

   package body Generic_LCG_32Only is

      -----------
      -- Reset --
      -----------

      procedure Reset (G: in out LCG_32Only; S: in U64) is
      begin
         G.s := U32(S and 16#FFFFFFFF#);
      end Reset;

      --------------
      -- Generate --
      --------------

      function Generate (G: in out LCG_32Only) return U32 is
      begin
         G.s := (G.s * Multiplier + Increment) mod Modulus;
         return G.s;
      end Generate;

   end Generic_LCG_32Only;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out LCG; S: in U64) is
   begin
      G.s := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out LCG) return U64 is
   begin
      G.s := (G.s * G.Multiplier + G.Increment) mod G.Modulus;
      return G.s;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out LCG_32Only; S: in U64) is
   begin
      G.s := U32(S and 16#FFFFFFFF#);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out LCG_32Only) return U32 is
   begin
      G.s := (G.s * G.Multiplier + G.Increment) mod G.Modulus;
      return G.s;
   end Generate;

end PRNG_Zoo.Linear_Congruential;
