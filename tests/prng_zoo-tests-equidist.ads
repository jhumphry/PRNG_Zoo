--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests.EquiDist is

   -- This test checks for equidistribution across 2**l divisions in 2**t dimensions
   -- with input of n bits
   type EquiDist(t, l, n : Positive) is limited new PRNG_Test with private;
   procedure Reset(T : in out EquiDist);
   procedure Feed(T : in out EquiDist; X : in U64) with Inline;
   procedure Compute_Result(T : in out EquiDist);
   function Result_Ready(T: EquiDist) return Boolean
     with Inline;
   function Passed(T : in EquiDist; p : in Long_Float := 0.01) return Boolean;
   function p(T : in EquiDist) return Long_Float;
   function Describe_Result(T : in EquiDist) return String;

   function Make_EquiDist(t, l: Positive; n: Positive := 64)
                          return access EquiDist;

private

   type EquiDist(t, l, n: Positive) is limited new PRNG_Test with
      record
         next_dimension : Positive;
         current : U64_array(1..t);
         bins : Counter_array_ptr;
         chi2_cdf_result : Long_Float := -1.0; -- indicates not yet computed
      end record;

end PRNG_Zoo.Tests.EquiDist;
