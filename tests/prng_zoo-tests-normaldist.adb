--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Stats;

with Ada.Characters.Latin_1;

package body PRNG_Zoo.Tests.NormalDist is

   -----------
   -- Reset --
   -----------

   procedure Reset (T : in out NormalTest) is
   begin
      Stats.Make_Normal_Bins(B => Binned(T));
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed (T : in out NormalTest; X : in Long_Float) is
      Success : Boolean := False;
   begin
      for I in T.Bin_Counts'First..(T.Bin_Counts'Last-1) loop
         if X < T.Bin_Boundary(I) then
            T.Bin_Counts(I) := T.Bin_Counts(I) + 1;
            Success := True;
            exit;
         end if;
      end loop;
      if not success then
         T.Bin_Counts(T.Bin_Counts'Last) := T.Bin_Counts(T.Bin_Counts'Last) + 1;
      end if;
   end Feed;

   ------------
   -- Result --
   ------------

   function Result (T : in NormalTest) return Test_Result_Ptr is
      Result : NormalTest_Result;
   begin
      Result.N := T.N;
      Result.chi2_value := Stats.Chi2_Value_Bins(Binned(T));
      Result.chi2_cdf_result := Stats.Chi2_CDF(Result.chi2_value,
                                               T.N - T.Distribution_DF);
      return new NormalTest_Result'(Result);
   end Result;

   ------------
   -- Passed --
   ------------

   function Passed
     (TR : in NormalTest_Result;
      p : in Long_Float := 0.01)
      return Boolean
   is
   begin
      return TR.chi2_cdf_result > p and TR.chi2_cdf_result < (1.0-p);
   end Passed;

   -------
   -- p --
   -------

   function p (TR : in NormalTest_Result) return Long_Float is
   begin
      return TR.chi2_cdf_result;
   end p;

   --------------
   -- Describe --
   --------------

   function Describe (TR : in NormalTest_Result) return String is
      LF : Character renames Ada.Characters.Latin_1.LF;
   begin
      return "Normal distribution test for " & Integer'Image(TR.N) &
        " bins with chi2 value := " & Long_Float'Image(TR.chi2_value) & LF &
      "corresponding to chi2 CDF := " & Long_Float'Image(TR.chi2_cdf_result);
   end Describe;

end PRNG_Zoo.Tests.NormalDist;
