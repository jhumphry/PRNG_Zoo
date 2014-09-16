--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Distributions;

generic
   with package Dist is new PRNG_Zoo.Distributions(<>);
package PRNG_Zoo.Tests.Distributions is

   type Test_Distribution is limited interface;
   procedure Reset(T : in out Test_Distribution) is abstract;
   procedure Feed(T : in out Test_Distribution; X : in Dist.Float_Type) is abstract;
   function Result(T : in Test_Distribution) return Test_Result'Class is abstract;

   function Run_Test(G : in out Dist.P;
                     D : in out Dist.Distribution'Class;
                     T : in out Test_Distribution'Class;
                     iterations : Positive := 1_000_000) return Test_Result'Class;

   type Test_Distribution_Ptr is access all Test_Distribution'Class;

   -- This test checks a supposed normal variate using N bins
   type NormalChi2(N : Positive) is new Binned(N) and Test_Distribution with null record;
   procedure Reset(T : in out NormalChi2);
   procedure Feed(T : in out NormalChi2; X : in Dist.Float_Type) with Inline;
   function Result(T : in NormalChi2) return Test_Result'Class;

   type NormalChi2_Result is new Test_Result with private;
   function Passed(TR : in NormalChi2_Result; p : in Long_Float := 0.01) return Boolean;
   function p(TR : in NormalChi2_Result) return Long_Float;
   function Describe(TR : in NormalChi2_Result) return String;

private

   type NormalChi2_Result is new Test_Result with
      record
         N : Positive;
         chi2_value : Long_Float;
         chi2_cdf_result : Long_Float;
      end record;

end PRNG_Zoo.Tests.Distributions;
