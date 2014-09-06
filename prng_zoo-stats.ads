--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Stats is

   -- Returns the error function
   function erf(x : Long_Float) return Long_Float;

   -- Returns the p-value corresponding to the probability of the null
   -- hypothesis
   function Z_Score(Z : Long_Float;
                    Two_Tailed: Boolean := True) return Long_Float;

   -- Returns True if the test statistic Z indicates that the null hypothesis
   -- is not ruled out to a confidence level of alpha
   function Z_Score(Z : Long_Float;
                    alpha : Long_Float := 0.05;
                    Two_Tailed: Boolean := True) return Boolean;

   -- Returns True if the test statistic Chi2 indicates that the null hypothesis
   -- is not ruled out to a confidence level of alpha with df degrees of freedom
   function Chi2_Test(Chi2 : Long_Float;
                      df : Positive;
                      alpha : Long_Float := 0.05) return Boolean;

   function Chi2_CDF(X : Long_Float;
                     df : Positive) return Long_Float;

   -- Return the Gamma function for N/2
   function Gamma_HalfN(N : Positive) return Long_Float;

end PRNG_Zoo.Stats;
