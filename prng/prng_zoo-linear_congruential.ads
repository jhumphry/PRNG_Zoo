--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Linear_Congruential is

   -- First, two generic subpackages for compile-time parameterisation
   generic
      Modulus : U64_Nonzero;
      Multiplier : U64_Nonzero;
      Increment : U64;
   package Generic_LCG is
      type LCG is new PRNG_64Only with private;
      function Strength(G: in LCG) return PRNG_Strength is (Low);
      function Constructor(Params : not null access PRNG_Parameters'Class) return LCG;
      procedure Reset(G: in out LCG; S: in U64);
      function Generate(G: in out LCG) return U64 with inline;

   private
      type LCG is new PRNG_64Only with
         record
            s : U64 := 1;
         end record;
      function Constructor(Params : not null access PRNG_Parameters'Class)
                                 return LCG is
        (LCG'(others => <>));

   end Generic_LCG;

   generic
      Modulus : U32_Nonzero;
      Multiplier : U32_Nonzero;
      Increment : U32;
   package Generic_LCG_32Only is

      type LCG_32Only is new PRNG_32Only with private;
      function Strength(G: in LCG_32Only) return PRNG_Strength is (Low);
      function Constructor(Params : not null access PRNG_Parameters'Class) return LCG_32Only;
      procedure Reset(G: in out LCG_32Only; S: in U64);
      function Generate(G: in out LCG_32Only) return U32 with inline;

   private

      type LCG_32Only is new PRNG_32Only with
         record
            s : U32 := 1;
         end record;

      function Constructor(Params : not null access PRNG_Parameters'Class)
                           return LCG_32Only is
        (LCG_32Only'(others => <>));

   end Generic_LCG_32Only;

   type LCG_Parameters is new PRNG_Parameters with
      record
         Modulus : U64_Nonzero;
         Multiplier : U64_Nonzero;
         Increment: U64;
         s : U64 := 1;
      end record;

   type LCG(Modulus : U64_Nonzero; Multiplier : U64_Nonzero; Increment: U64) is new PRNG with private;
   function Strength(G: in     LCG) return PRNG_Strength is (Low);
   function Constructor(Params : not null access PRNG_Parameters'Class) return LCG;
   procedure Reset(G: in out LCG; S: in U64);
   function Generate(G: in out LCG) return U64 with inline;

   type LCG_32Only_Parameters is new PRNG_Parameters with
      record
         Modulus : U32_Nonzero;
         Multiplier : U32_Nonzero;
         Increment: U32;
         s : U32;
      end record;

   type LCG_32Only(Modulus : U32_Nonzero; Multiplier : U32_Nonzero; Increment: U32) is
     new PRNG_32Only with private;
   function Strength(G: in LCG_32Only) return PRNG_Strength is (Low);
   function Constructor(Params : not null access PRNG_Parameters'Class) return LCG_32Only;
   procedure Reset(G: in out LCG_32Only; S: in U64);
   function Generate(G: in out LCG_32Only) return U32 with inline;

private

   type LCG(Modulus : U64_Nonzero; Multiplier : U64_Nonzero; Increment: U64) is new PRNG_64Only with
      record
         s : U64 := 1;
      end record;

   type LCG_32Only(Modulus : U32_Nonzero; Multiplier : U32_Nonzero; Increment: U32) is new PRNG_32Only with
      record
         s : U32 := 1;
      end record;

end PRNG_Zoo.Linear_Congruential;


