--
-- PRNG Zoo
-- Copyright (c) 2014 - 2015, James Humphry
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
-- REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
-- AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
-- INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
-- LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
-- OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
-- PERFORMANCE OF THIS SOFTWARE.

with PRNG_Zoo.Tests;

package PRNG_Zoo.Stats is

   -- Returns the adjusted p-value to use for testing n results so that the
   -- overall chance of a false positive is p.
   function Adjusted_alpha(alpha : Long_Float; N : Positive) return Long_Float;

   -- Returns the error function
   -- Based on (Abramowitz and Stegun, 1972) 7.1.26
   function erf(x : Long_Float) return Long_Float;

   -- Returns the inverse error function
   -- Based on the note by (Winitzki, 2008)
   -- Not quite as accurate as the erf function given above
   function erfi(x : Long_Float) return Long_Float;

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
   -- Based on a version of (Abramowitz and Stegun, 1972) 26.4.6, see notes
   function Chi2_Test(Chi2 : Long_Float;
                      df : Positive;
                      alpha : Long_Float := 0.05) return Boolean;

   function Chi2_CDF(X : Long_Float;
                     K : Positive;
                     epsilon : Long_Float := 1.0E-6) return Long_Float;

   -- Return the Gamma function for N/2
   function Gamma_HalfN(N : Positive) return Long_Float;


   -- Return the logarithm of the Gamma function for N/2
   function Log_Gamma_HalfN(N : Positive) return Long_Float;

   -- Compute the Chi2 test for an array of counters representing the binned
   -- results of some sort of test
   function Chi2_Value_Bins(B : Tests.Binned) return Long_Float;

   function Chi2_CDF_Bins(B : Tests.Binned) return Long_Float;

   function Chi2_Bins_Test(B : Tests.Binned;
                          alpha : Long_Float := 0.05) return Boolean;

   -- Make a set of bins suitable for testing variates that are supposed to
   -- be from the normal distribution
   procedure Make_Normal_Bins(B : in out Tests.Binned);

end PRNG_Zoo.Stats;
