--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;

package body PRNG_Zoo.Stats is

   ---------
   -- erf --
   ---------

   function erf(x : Long_Float) return Long_Float is
      -- uses Abramowitz and Stegun 7.1.26 page 299
      p : constant Long_Float  := 0.32759_11;
      a1 : constant Long_float := 0.25482_9592;
      a2 : constant Long_Float := -0.28449_6736;
      a3 : constant Long_Float := 1.42141_3741;
      a4 : constant Long_Float := -1.45315_2027;
      a5 : constant Long_Float := 1.06140_5429;
      z : Long_Float := abs(x);
      t : Long_Float := 1.0 / (1.0 + p * z);
      u : Long_Float := t;
      s : Long_Float;
   begin
      s := a1 * u;
      u := u * t;
      s := s + a2 * u;
      u := u * t;
      s := s + a3 * u;
      u := u * t;
      s := s + a4 * u;
      u := u * t;
      s := s + a5 * u;
      return Long_Float'Copy_Sign(1.0 - s * exp(-z*z), x);
   end erf;

   -------------
   -- Z_Score --
   -------------

   function Z_Score
     (Z : Long_Float;
      Two_Tailed: Boolean := True)
      return Long_Float
   is
      sqrt2 : constant Long_Float := 1.41421_35623_73095_04880;
      sf : Long_Float;
   begin
      sf := 0.5 * (1.0 + erf(-abs(Z)/sqrt2));

      if Two_Tailed then
         sf := sf * 2.0;
      end if;

      return sf;
   end Z_Score;

   function Z_Score
     (Z : Long_Float;
      alpha : Long_Float := 0.05;
      Two_Tailed: Boolean := True)
      return Boolean
   is
   begin
      return Z_Score(Z, Two_Tailed) > alpha;
   end Z_Score;

   ---------------
   -- Chi2_Test --
   ---------------

   function Chi2_Test(Chi2 : Long_Float;
                      df : Positive;
                      alpha : Long_Float := 0.05) return Boolean is
      cdf : Long_Float := Chi2_CDF(Chi2, df);
   begin
      return cdf > alpha and cdf < (1.0-alpha);
   end Chi2_Test;

   --------------
   -- Chi2_CDF --
   --------------

   function Chi2_CDF(X : Long_Float;
                     df : Positive) return Long_Float is

      Z : Long_Float := X / 2.0;
      k : Natural := df / 2;
      k_odd : Boolean := ((df mod 2)=1);
      c : Long_Float;
      num, den, sum : Long_Float;
      I : Positive;
      Renorm : Integer;
   begin
      c := Z ** k * exp(-Z);
      num := 1.0;
      den := Gamma_HalfN(df + 2);
      sum := num / den;

      if k_odd then
         c := c * sqrt(Z);
         I := 1;
         loop
            num := num * Z * 2.0;
            den := den * (2.0 * Long_Float(I + k) + 1.0);
            sum := sum + num / den;
            exit when I > df and I > 25;
            I := I + 1;

            -- renormalise the fraction to prevent overflow
            renorm := Integer'Min(Long_Float'Exponent(den),Long_Float'Exponent(num));
            num := Long_Float'Scaling(num, -renorm);
            den := Long_Float'Scaling(den, -renorm);
         end loop;
      else
         I := 1;
         loop
            num := num * Z;
            den := den * Long_Float(I + k);
            sum := sum + num / den;
            exit when I > df and I > 25;
            I := I + 1;

            -- renormalise the fraction to prevent overflow
            renorm := Integer'Min(Long_Float'Exponent(den),Long_Float'Exponent(num));
            num := Long_Float'Scaling(num, -renorm);
            den := Long_Float'Scaling(den, -renorm);
         end loop;
      end if;
      return c * sum;
   end Chi2_CDF;


   -----------------
   -- Gamma_HalfN --
   -----------------

   -- Return the Gamma function for N/2
   function Gamma_HalfN(N : Positive) return Long_Float is
      Result : Long_Float := 1.0;
      sqrt_pi : constant Long_Float := 1.77245_38509_05516_02729;
      num : Long_Float := 1.0;
      den : Long_Float := 2.0;
      renorm : Integer;
   begin
      if N mod 2 = 0 then
         -- If N/2 is integral, gamma(N/2) is simply (N/2-1)!
         for I in Integer range 1..(N/2-1) loop
            Result := Result * Long_Float(I);
         end loop;
         return Result;
      else
         -- If N/2 is half-integral, we use the formula
         -- gamma(N + 1/2) = sqrt(pi) * (2n-1)!!/(2**n)
         if N = 1 then
            return sqrt_pi;
         end if;
         for I in Integer range 1..(N/2-1) loop
            num := num * (2.0 * Long_Float(I) + 1.0);
            den := den * 2.0;

            -- renormalise the fraction to prevent overflow
            renorm := Integer'Min(Long_Float'Exponent(den),Long_Float'Exponent(num));
            num := Long_Float'Scaling(num, -renorm);
            den := Long_Float'Scaling(den, -renorm);
         end loop;
         return sqrt_pi * num / den;
      end if;
   end Gamma_HalfN;

end PRNG_Zoo.Stats;
