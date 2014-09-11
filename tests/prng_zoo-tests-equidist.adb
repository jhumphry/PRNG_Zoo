--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Stats;

with Ada.Text_IO;

package body PRNG_Zoo.Tests.EquiDist is

   -----------
   -- Reset --
   -----------

   procedure Reset (T : in out EquiDist) is
   begin
      T.next_dimension := 1;
      T.current := (others => 0);
      T.bins.all := (others => 0);
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed (T : in out EquiDist; X : in U64) is
      stride : Positive;
      position : Natural;
   begin
      T.current(T.next_dimension) := Shift_Right(X, T.n-T.l);
      if T.next_dimension = T.t then
         stride := 1;
         position := 0;
         for I in T.current'Range loop
            position := position + Natural(T.current(I)) * stride;
            stride := stride * 2 ** T.l;
         end loop;
         T.bins(position+1) := T.bins(position+1) + 1;
         T.next_dimension := 1;
      else
         T.next_dimension := T.next_dimension + 1;
      end if;
   end Feed;

   ------------
   -- Result --
   ------------

   function Result (T : in EquiDist) return Test_Result_Ptr is
      N : Positive := 2 ** (T.t * T.l);
      Expected : Long_Float := 1.0 / Long_Float(N);
      B : Binned(N);
      Result : EquiDist_Result;

      use Ada.Text_IO;
   begin
      B.Bin_Counts := T.bins.all;
      B.Bin_Expected := (others => Expected);
      B.Distribution_DF := 1;

      Result.t := T.t;
      Result.l := T.l;
      Result.chi2_cdf_result := Stats.Chi2_CDF_Bins(B);

      return new EquiDist_Result'(Result);
   end Result;

   -------------------
   -- Make_EquiDist --
   -------------------

   function Make_EquiDist (t, l: Positive; n : Positive := 64) return Test_Ptr is
      Result : access EquiDist;
      Num_Bins : Natural := 2**(t * l);
   begin
      if l >= n then
         raise Constraint_Error
           with "Cannot have finer subdivisions per dimension than the number of input bits";
      end if;

      Result := new EquiDist(t, l, n);
      Result.next_dimension := 1;
      Result.current := (others => 0);
      Result.bins := new Counter_array(1..Num_Bins);
      Result.bins.all := (others => 0);
      return Test_Ptr(Result);
   end Make_EquiDist;

   ------------
   -- Passed --
   ------------

   function Passed
     (TR : in EquiDist_Result;
      p : in Long_Float := 0.01)
      return Boolean
   is
   begin
      return TR.chi2_cdf_result > p and TR.chi2_cdf_result < (1.0-p);
   end Passed;

   -------
   -- p --
   -------

   function p (TR : in EquiDist_Result) return Long_Float is
   begin
      return TR.chi2_cdf_result;
   end p;

   --------------
   -- Describe --
   --------------

   function Describe (TR : in EquiDist_Result) return String is
   begin
      return "Equidistribution test (" & Integer'Image(TR.t) &
        "," & Integer'Image(TR.l) & ") with result := " &
        Long_Float'Image(TR.chi2_cdf_result);
   end Describe;

end PRNG_Zoo.Tests.EquiDist;
