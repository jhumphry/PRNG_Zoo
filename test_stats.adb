--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;
with PRNG_Zoo.Stats;

with Ada.Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Long_Float_Text_IO, Ada.Integer_Text_IO;

procedure test_stats is
   procedure test_chi2_cdf(X : Long_Float; df : Positive; expect : Long_Float) is
   begin
      Put("Chi2 CDF for X = ");
      Put(X);
      Put(" df = ");
      Put(df);
      New_Line;
      Put("Result = ");
      Put(Stats.Chi2_CDF(X, df));
      Put(" expected = ");
      Put(expect);
      New_Line(2);
   end test_chi2_cdf;

begin
   Put_Line("Testing Gamma_Half");
   for I in 1..10 loop
      Put("Z = ");
      Put(I);
      Put(" Gamma(Z/2) = ");
      Put(Stats.Gamma_HalfN(I));
      New_Line;
   end loop;
   New_Line(2);

   Put_Line("Testing Chi2");
   test_chi2_cdf(9.210340371976180, 2, 0.99);
   test_chi2_cdf(7.814727903251176, 3, 0.95);
   test_chi2_cdf(0.554298076728276, 5, 0.01);
   test_chi2_cdf(70.0, 50, 0.96762589022646417);
   test_chi2_cdf(311.560343126936004, 256, 0.99);
   test_chi2_cdf(190.867048914205071, 255, 0.001);
   New_Line(2);

   Put("Z-Score(-2.0) :"); Put(Stats.Z_Score(-2.0)); New_Line;
   Put("Z-Score(-1.0) :"); Put(Stats.Z_Score(-1.0)); New_Line;
   Put("Z-Score(0.0) :"); Put(Stats.Z_Score(0.0)); New_Line;
   Put("Z-Score(1.0) :"); Put(Stats.Z_Score(1.0)); New_Line;
   Put("Z-Score(2.0) :"); Put(Stats.Z_Score(2.0)); New_Line;
end test_stats;
