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

   function Result (T: in Bit_Counter;
                    p : in Long_Float := 0.01) return Test_Result_Ptr is
      R : Bit_Counter_Result;
      Recip_Sqrt_N : Long_Float := 1.0 / Sqrt(Long_Float(T.N));
      Expected_Ones : Long_Float := Long_Float(T.N) / 2.0;
      Z : Long_Float;
   begin
      R.p := p;
      R.N := T.N;

      R.Total_Bits := 0;
      for I in T.B'Range loop
         R.Total_Bits := R.Total_Bits + T.B(I);
      end loop;
      Z := (Long_Float(R.Total_Bits) - Long_Float(T.N*32)) / sqrt(Long_Float(T.N*64));
      R.Total_Bits_Pass := Stats.Z_Score(Z, p, True);

      for I in T.B'Range loop
         Z := (Long_Float(T.B(I)) - Expected_Ones) * Recip_Sqrt_N;
         R.Each_Bit_Pass(I) := Stats.Z_Score(Z, p, True);
      end loop;

      R.All_Pass := R.Total_Bits_Pass and
        (for all I in R.Each_Bit_Pass'Range => R.Each_Bit_Pass(I));

      return new Bit_Counter_Result'(R);

   end Result;

   ------------
   -- Passed --
   ------------

   function Passed
     (TR : in Bit_Counter_Result)
      return Boolean
   is
   begin
      return TR.All_Pass;
   end Passed;

   --------------
   -- Describe --
   --------------

   function Describe (TR : in Bit_Counter_Result) return String is
      Totals : String := Counter'Image(TR.Total_Bits) & " of " & Counter'Image(TR.N*64);
      Total_Result : String := (if TR.Total_Bits_Pass then "Pass" else "Fail");
      Bits : String(1..64);
      LF : Character renames Ada.Characters.Latin_1.LF;
   begin
      for I in Bits'Range loop
         if TR.Each_Bit_Pass(65 - I) then
            Bits(I) := 'Y';
         else
            Bits(I) := 'N';
         end if;
      end loop;

      return "Bit Counter with p-value: " & Long_Float'Image(TR.p) & ": " & LF &
        Totals &  " bits set in total for a Z-Score: " & Total_Result & LF &
        "Individual bit Z-score results (MSB -> LSB): " & Bits;
   end Describe;

end PRNG_Zoo.Tests.Bits;
