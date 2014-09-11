--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests.NormalDist is

   -- This test checks a supposed normal variate using N bins
   type NormalTest(N : Positive) is new Binned(N) and Test_Distribution with
     null record;
   procedure Reset(T : in out NormalTest);
   procedure Feed(T : in out NormalTest; X : in Long_Float) with Inline;
   function Result(T : in NormalTest) return Test_Result_Ptr;

   type NormalTest_Result is new Test_Result with private;
   function Passed(TR : in NormalTest_Result; p : in Long_Float := 0.01) return Boolean;
   function p(TR : in NormalTest_Result) return Long_Float;
   function Describe(TR : in NormalTest_Result) return String;

private

   type NormalTest_Result is new Test_Result with
      record
         N : Positive;
         chi2_value : Long_Float;
         chi2_cdf_result : Long_Float;
      end record;

end PRNG_Zoo.Tests.NormalDist;
