--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests.EquiDist is

   type EquiDist(t, l: Positive) is limited new Test with private;
   procedure Reset(T : in out EquiDist);
   procedure Feed(T : in out EquiDist; X : in U64) with Inline;
   function Result(T : in EquiDist) return Test_Result_Ptr;

   function Make_EquiDist(t, l: Positive) return Test_Ptr;

   type EquiDist_Result is new Test_Result with private;
   function Passed(TR : in EquiDist_Result; p : in Long_Float := 0.01) return Boolean;
   function p(TR : in EquiDist_Result) return Long_Float;
   function Describe(TR : in EquiDist_Result) return String;

private

   type EquiDist(t, l: Positive) is limited new Test with
      record
         next_dimension : Positive;
         current : U64_array(1..t);
         bins : Counter_array_ptr;
      end record;

   type EquiDist_Result is new Test_Result with
      record
         t, l: Positive;
         chi2_cdf_result : Long_Float;
      end record;

end PRNG_Zoo.Tests.EquiDist;
