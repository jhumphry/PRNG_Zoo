--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Stats.Tables;

package body PRNG_Zoo.Stats is

   -------------
   -- Z_Score --
   -------------

   function Z_Score
     (Z : Long_Float;
      alpha : Long_Float := 0.05;
      Two_Tailed: Boolean := True)
      return Boolean
   is
      lower : Positive := Integer(Long_Float'Floor(abs(Z)*100.0)) + 1;
      upper : Positive := lower + 1;
      residue : Long_Float := abs(Z)*100.0 - Long_Float'Floor(abs(Z)*100.0);
      sf : Long_Float;
   begin
      if upper > 500 then
         return False;
      end if;

      sf := (1.0 - residue ) * Tables.Normal(lower) + residue * Tables.Normal(upper);

      if Two_Tailed then
         sf := sf * 2.0;
      end if;

      return sf > alpha;
   end Z_Score;

   ---------------
   -- Chi2_Test --
   ---------------

   function Chi2_Test(Chi2 : Long_Float;
                      df : Positive;
                      alpha : Long_Float := 0.05) return Boolean is
      Index : Positive := 5;
   begin

      if df > Tables.Chi2_Upper'Last then
         raise Constraint_Error with "Too many degrees of freedom: " & Integer'Image(df);
      end if;

      for I in Tables.Chi2_Critical_Values'Range loop
         if Tables.Chi2_Critical_Values(I) = alpha then
            Index := I;
         end if;
      end loop;
      if Index = 5 then
         raise Constraint_Error with "Chi2 test does not support confidence level: " & Long_Float'Image(alpha);
      end if;

      return (Chi2 <= Tables.Chi2_Upper(df, Index) and Chi2 >= Tables.Chi2_Lower(df, Index));
   end Chi2_Test;

end PRNG_Zoo.Stats;
