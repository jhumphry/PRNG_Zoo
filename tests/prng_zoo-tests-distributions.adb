--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Stats;

with Ada.Characters.Latin_1;

package body PRNG_Zoo.Tests.Distributions is

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test(G : in out PRNG'Class;
                      D : in out Dist.Distribution'Class;
                      T : in out Test_Distribution'Class;
                      iterations : Positive := 1_000_000) is
   begin
      for I in 1..iterations loop
         T.Feed(D.Generate(G));
      end loop;
      T.Compute_Result;
   end Run_Test;

   -----------
   -- Reset --
   -----------

   procedure Reset (T : in out NormalChi2) is
   begin
      Stats.Make_Normal_Bins(B => Binned(T));
      T.chi2_cdf_result := -1.0;
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed
     (T : in out NormalChi2;
      X : in Dist.Float_Type)
   is
      Success : Boolean := False;
   begin
      for I in T.Bin_Counts'First..(T.Bin_Counts'Last-1) loop
         if Long_Float(X) < T.Bin_Boundary(I) then
            T.Bin_Counts(I) := T.Bin_Counts(I) + 1;
            Success := True;
            exit;
         end if;
      end loop;
      if not success then
         T.Bin_Counts(T.Bin_Counts'Last) := T.Bin_Counts(T.Bin_Counts'Last) + 1;
      end if;
   end Feed;

   --------------------
   -- Compute_Result --
   --------------------

   procedure Compute_Result (T : in out NormalChi2) is
   begin
      T.chi2_value := Stats.Chi2_Value_Bins(Binned(T));
      T.chi2_cdf_result := Stats.Chi2_CDF(T.chi2_value,
                                          T.N - T.Distribution_DF);
   end Compute_Result;

   ------------------
   -- Result_Ready --
   ------------------

   function Result_Ready(T: NormalChi2) return Boolean is
   begin
      return T.chi2_cdf_result /= -1.0;
   end Result_Ready;

   ------------
   -- Passed --
   ------------

   function Passed
     (T : in NormalChi2;
      p : in Long_Float := 0.01)
      return Boolean
   is
   begin
      return T.chi2_cdf_result > p and T.chi2_cdf_result < (1.0-p);
   end Passed;

   -------
   -- p --
   -------

   function p (T : in NormalChi2) return Long_Float is
   begin
      return T.chi2_cdf_result;
   end p;

   ---------------------
   -- Describe_Result --
   ---------------------

   function Describe_Result (T : in NormalChi2) return String is
      LF : Character renames Ada.Characters.Latin_1.LF;
   begin
      return "Normal Chi2 distribution test for " & Integer'Image(T.N) &
        " bins with chi2 value := " & Long_Float'Image(T.chi2_value) & LF &
      "corresponding to chi2 CDF := " & Long_Float'Image(T.chi2_cdf_result);
   end Describe_Result;

end PRNG_Zoo.Tests.Distributions;
