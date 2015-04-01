--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Ada.Numerics.Generic_Elementary_Functions;

generic
   type Float_Type is digits <>;
   scale : Float_Type'Base;
package PRNG_Zoo.Distributions is
   package EF is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => Float_Type);
   use EF;

   type Distribution is interface;
   procedure Reset(D: in out Distribution) is null;
   function Generate(D: in out Distribution; G : in out PRNG'Class) return Float_Type is abstract;

   type Uniform01 is new Distribution with null record;
   function Generate(D: in out Uniform01; G: in out PRNG'Class) return Float_Type with inline;

   type Uniform is new Distribution with
      record
         a : Float_Type := 0.0;
         b : Float_Type := 1.0;
      end record;
   function Generate(D: in out Uniform; G: in out PRNG'Class) return Float_Type with inline;

   type Normal_Distribution is interface and Distribution;

   type Normal_12_6 is new Normal_Distribution with null record;
   function Generate(D: in out Normal_12_6; G: in out PRNG'Class) return Float_Type;

   type Normal_Box_Mueller is new Normal_Distribution with
      record
         Loaded : Boolean := False;
         r2 : Float_Type := 0.0;
      end record;
   procedure Reset(D: in out Normal_Box_Mueller);
   function Generate(D: in out Normal_Box_Mueller; G: in out PRNG'Class) return Float_Type;

   -- From (Marsaglia and Tsang 1998a) with guidance from (Thomas et al., 2007)
   type Normal_Monty_Python is new Normal_Distribution with null record;
   function Generate(D: in out Normal_Monty_Python; G: in out PRNG'Class) return Float_Type;

   type Exponential is new Distribution with
      record
         theta : Float_Type := 1.0;
      end record;
   function Generate(D: in out Exponential; G: in out PRNG'Class) return Float_Type with inline;

end PRNG_Zoo.Distributions;
