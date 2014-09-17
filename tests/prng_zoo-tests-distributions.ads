--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Distributions;

generic
   with package Dist is new PRNG_Zoo.Distributions(<>);
package PRNG_Zoo.Tests.Distributions is

   type Test_Distribution is limited interface and Test;
   procedure Reset(T : in out Test_Distribution) is abstract;
   procedure Feed(T : in out Test_Distribution; X : in Dist.Float_Type) is abstract;
   procedure Compute_Result(T : in out Test_Distribution) is abstract;

   procedure Run_Test(G : in out Dist.P;
                      D : in out Dist.Distribution'Class;
                      T : in out Test_Distribution'Class;
                      iterations : Positive := 1_000_000);

   type Test_Distribution_Ptr is access all Test_Distribution'Class;

   -- This test checks a supposed normal variate using N bins
   type NormalChi2(N : Positive) is new Binned and Test_Distribution with private;
   procedure Reset(T : in out NormalChi2);
   procedure Feed(T : in out NormalChi2; X : in Dist.Float_Type) with Inline;
   procedure Compute_Result(T : in out NormalChi2);
   function Result_Ready(T: NormalChi2) return Boolean;
   function Passed(T : in NormalChi2; p : in Long_Float := 0.01) return Boolean;
   function p(T : in NormalChi2) return Long_Float;
   function Describe_Result(T : in NormalChi2) return String;

private

   type NormalChi2(N : Positive) is new Binned(N) and Test_Distribution with
      record
         chi2_value : Long_Float;
         chi2_cdf_result : Long_Float := -1.0;
      end record;

end PRNG_Zoo.Tests.Distributions;
