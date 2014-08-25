--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Ada.Numerics.Generic_Elementary_Functions;

generic
   type Float_Type is digits <>;
   type P is new PRNG with private;
package PRNG_Zoo.Distributions is
   package EF is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float_Type);
   use EF;

   type Distribution is abstract new P with null record;
   function Strength(D: in Distribution) return PRNG_Strength is (Strength(P(D)));
   function Generate(D: in out Distribution) return U64 is (Generate(P(D)));
   procedure Reset(D: in out Distribution; S: in U64) is abstract;
   function Generate(D: in out Distribution) return Float_Type is abstract;

   type Uniform01 is new Distribution with null record;
   procedure Reset(D: in out Uniform01; S: in U64);
   function Generate(D: in out Uniform01) return Float_Type;

   type Uniform is new Distribution with
      record
         a : Float_Type := 0.0;
         b : Float_Type := 1.0;
      end record;
   procedure Reset(D: in out Uniform; S: in U64);
   function Generate(D: in out Uniform) return Float_Type;

   type Normal_12_6 is new Distribution with null record;
   procedure Reset(D: in out Normal_12_6; S: in U64);
   function Generate(D: in out Normal_12_6) return Float_Type;

   type Normal_Box_Mueller is new Distribution with
      record
         Loaded : Boolean := False;
         r2 : Float_Type := 0.0;
      end record;
   procedure Reset(D: in out Normal_Box_Mueller; S: in U64);
   function Generate(D: in out Normal_Box_Mueller) return Float_Type;


end PRNG_Zoo.Distributions;
