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

   -----------------
   -- Constructor --
   -----------------

   function Constructor(Params : not null access PRNG_Parameters'Class) return LCG is
      P : LCG_Parameters;
   begin
      if Params.all in LCG_Parameters then
         P := LCG_Parameters(Params.all);
         return LCG'(Modulus      => P.Modulus,
                     Multiplier   => P.Multiplier,
                     Increment    => P.Increment,
                     Usable_Width => P.Usable_Width,
                     s            => P.s);
      else
         raise Invalid_Parameters;
      end if;
   end Constructor;

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

   -----------------
   -- Constructor --
   -----------------

   function Constructor(Params : not null access PRNG_Parameters'Class) return LCG_32Only is
      P : LCG_32Only_Parameters;
   begin
      if Params.all in LCG_32Only_Parameters then
         P := LCG_32Only_Parameters(Params.all);
         return LCG_32Only'(Modulus      => P.Modulus,
                            Multiplier   => P.Multiplier,
                            Increment    => P.Increment,
                            Usable_Width => P.Usable_Width,
                            s            => P.s);
      else
         raise Invalid_Parameters;
      end if;
   end Constructor;

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
