--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo.Stats;

with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;

with Ada.Characters.Latin_1;

package body PRNG_Zoo.Tests.Bits is

   -----------
   -- Reset --
   -----------

   procedure Reset (T: in out Bit_Counter) is
   begin
      T.N := 0;
      T.B := (others => 0);
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed (T: in out Bit_Counter; X : U64) is
      Y : U64 := X;
   begin
      T.N := T.N + 1;
      T.B(1) := T.B(1) + Counter(Y and 1);
      for I in 2..64 loop
         Y := Shift_Right(Y, 1);
         T.B(I) := T.B(I) + Counter(Y and 1);
      end loop;
   end Feed;

   ------------
   -- Result --
   ------------

   function Result (T: in Bit_Counter; Width : Positive) return Test_Result_Ptr is
      R : Bit_Counter_Result;
      Recip_Sqrt_N : Long_Float := 1.0 / Sqrt(Long_Float(T.N));
      Expected_Ones : Long_Float := Long_Float(T.N) / 2.0;
      Z : Long_Float;
   begin

      if T.N = 0 then
         R.Width := Width;
         R.N := 0;
         R.Total_Bits := 0;
         R.Total_Bits_p_value := 0.0;
         R.Each_Bit_p_value := (others => 0.0);
         return new Bit_Counter_Result'(R);
      end if;

      R.Width := Width;
      R.N := T.N;

      R.Total_Bits := 0;
      for I in 1..Width loop
         R.Total_Bits := R.Total_Bits + T.B(I);
      end loop;
      Z := (Long_Float(R.Total_Bits) - Long_Float(T.N*Width / 2)) / sqrt(Long_Float(T.N*Width));
      R.Total_Bits_p_value := Stats.Z_Score(Z, True);

      for I in T.B'Range loop
         Z := (Long_Float(T.B(I)) - Expected_Ones) * Recip_Sqrt_N;
         R.Each_Bit_p_value(I) := Stats.Z_Score(Z, True);
      end loop;

      return new Bit_Counter_Result'(R);

   end Result;

   ------------
   -- Passed --
   ------------

   function Passed
     (TR : in Bit_Counter_Result; p : in Long_Float := 0.01)
      return Boolean
   is
   begin
      return TR.Total_Bits_p_value > p and
        (for all I in 1..TR.Width => TR.Each_Bit_p_value(I) > p);
   end Passed;

   -------
   -- p --
   -------

   function p(TR : in Bit_Counter_Result) return Long_Float
   is
   begin
      return TR.Total_Bits_p_value;
   end p;

   --------------
   -- Describe --
   --------------

   function Describe (TR : in Bit_Counter_Result) return String is
      Totals : String := Counter'Image(TR.Total_Bits) & " of " & Counter'Image(TR.N * TR.Width);
      Total_Result : String := Long_Float'Image(TR.Total_Bits_p_value);
      Bits : String(1..TR.Width);
      p_value : Long_Float;
      LF : Character renames Ada.Characters.Latin_1.LF;
   begin

      if TR.N = 0 then
         return "Empty Bit Counter";
      end if;

      for I in Bits'Range loop
         p_value := TR.Each_Bit_p_value(TR.Width + 1 - I);
         if p_value < 1.0E-6 then
            Bits(I) := 'd';
         elsif p_value < 1.0E-3 then
            Bits(I) := 'c';
         elsif p_value < 0.01 then
            Bits(I) := 'b';
         elsif p_value < 0.05 then
            Bits(I) := 'b';
         else
            Bits(I) := '-';
         end if;
      end loop;

      return "Bit Counter with width " & Integer'Image(TR.Width) & ":" & LF &
        Totals &  " bits set in total for a Z-Score: " & Total_Result & LF &
        "Individual bit Z-score results MSB -> LSB, failures ranked a (@ 5%) to d (@ 1E-6): "
        & LF & Bits;
   end Describe;

end PRNG_Zoo.Tests.Bits;
