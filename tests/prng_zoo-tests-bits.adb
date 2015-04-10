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
      T := (Width => T.Width,
            N => 0,
            B => (others => 0),
            Ready => False,
            Total_Bits => 0,
            Total_Bits_p_value => 0.0,
            Each_Bit_p_value => (others => 0.0)
           );
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed (T: in out Bit_Counter; X : U64) is
      Y : U64 := X;
   begin
      T.N := T.N + 1;
      for I in 1..(T.Width-1) loop
         T.B(I) := T.B(I) + Counter(Y and 1);
         Y := Shift_Right(Y, 1);
      end loop;
      T.B(T.Width) := T.B(T.Width) + Counter(Y and 1);
   end Feed;

   --------------------
   -- Compute_Result --
   --------------------

   procedure Compute_Result (T : in out Bit_Counter) is
      Recip_Sqrt_N : Long_Float := 1.0 / Sqrt(Long_Float(T.N));
      Expected_Ones : Long_Float := Long_Float(T.N) / 2.0;
      Z : Long_Float;
   begin

      if T.N = 0 then
         raise Insufficient_Data with "No data has been input.";
      end if;

      T.Total_Bits := 0;
      for I in T.B'Range loop
         T.Total_Bits := T.Total_Bits + T.B(I);
      end loop;
      Z := (Long_Float(T.Total_Bits) - Long_Float(T.N*T.Width / 2)) / sqrt(Long_Float(T.N*T.Width));
      T.Total_Bits_p_value := Stats.Z_Score(Z, True);

      for I in T.B'Range loop
         Z := (Long_Float(T.B(I)) - Expected_Ones) * Recip_Sqrt_N;
         T.Each_Bit_p_value(I) := Stats.Z_Score(Z, True);
      end loop;

      T.Ready := True;
   end Compute_Result;

   ------------------
   -- Result_Ready --
   ------------------

   function Result_Ready (T: Bit_Counter) return Boolean is (T.Ready);

   ------------
   -- Passed --
   ------------

   function Passed
     (T : in Bit_Counter; p : in Long_Float := 0.01)
      return Boolean
   is
   begin

      return T.Total_Bits_p_value > p and
        (for all I in 1..T.Width =>
           T.Each_Bit_p_value(I) > Stats.Adjusted_alpha(p, T.Width));
   end Passed;

   -------
   -- p --
   -------

   function p (T : in Bit_Counter) return Long_Float is (T.Total_Bits_p_value);

   --------------
   -- Describe --
   --------------

   function Describe_Result (T : in Bit_Counter) return String is
      Totals : String := Counter'Image(T.Total_Bits) & " of " & Counter'Image(T.N * T.Width);
      Total_Result : String := Long_Float'Image(T.Total_Bits_p_value);
      Bits : String(1..T.Width);
      p_value : Long_Float;
      LF : Character renames Ada.Characters.Latin_1.LF;
   begin

      for I in Bits'Range loop
         p_value := T.Each_Bit_p_value(T.Width + 1 - I);
         if p_value < 1.0E-6 then
            Bits(I) := 'd';
         elsif p_value < 1.0E-3 then
            Bits(I) := 'c';
         elsif p_value < 0.01 then
            Bits(I) := 'b';
         elsif p_value < 0.05 then
            Bits(I) := 'a';
         else
            Bits(I) := '-';
         end if;
      end loop;

      return "Bit Counter with width " & Integer'Image(T.Width) & ":" & LF &
        Totals &  " bits set in total for a Z-Score: " & Total_Result & LF &
        "Individual bit Z-score results MSB->LSB, failures ranked a (@ 5%) to d (@ 1E-6): "
        & LF & Bits;
   end Describe_Result;

end PRNG_Zoo.Tests.Bits;
