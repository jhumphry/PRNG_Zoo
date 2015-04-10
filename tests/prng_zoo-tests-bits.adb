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


   -----------
   -- Reset --
   -----------

   procedure Reset (T: in out WW_Runs) is
   begin
      T := (Width => T.Width,
            N => 0,
            Current_Run => 0,
            Total_0 => (others => 0),
            Total_1 => (others => 0),
            Runs => (others => 1),
            Ready => False,
            Each_Bit_p_value => (others => 0.0)
           );
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed (T: in out WW_Runs; X : U64) is
      Y : U64 := X;
      Syndrome : U64;
   begin
      if T.N > 0 then
         Syndrome := T.Current_Run xor X;
         for I in 1..(T.Width) loop
            if (Syndrome and 1) = 1 then
               T.Runs(I) := T.Runs(I) + 1;
            end if;
            Syndrome := Shift_Right(Syndrome, 1);
         end loop;
      end if;

      for I in 1..T.Width loop
         if (Y and 1) = 1 then
            T.Total_1(I) := T.Total_1(I) + 1;
         else
            T.Total_0(I) := T.Total_0(I) + 1;
         end if;
         Y := Shift_Right(Y, 1);
      end loop;

      T.N := T.N + 1;
      T.Current_Run := X;
   end Feed;

   --------------------
   -- Compute_Result --
   --------------------

   procedure Compute_Result (T : in out WW_Runs) is
      function Mean (N, N1, N0 : Counter) return Long_Float is
        (2.0 * Long_Float(N1) * Long_Float(N0) / Long_Float(N) + 1.0);

      function Var (N, N1, N0 : Counter) return Long_Float is
        ((Mean(N, N1, N0) - 1.0) * (Mean(N, N1, N0) - 2.0) / (Long_Float(N) - 1.0));

      mu, sigma, Z : Long_Float;

   begin

      if T.N = 0 then
         raise Insufficient_Data with "No data has been input.";
      end if;

      for I in 1..T.Width loop

         mu := Mean (T.N, T.Total_0(I), T.Total_1(I));
         sigma := Sqrt( Var(T.N, T.Total_0(I), T.Total_1(I)) );

         if sigma /= 0.0 then
         Z := (Long_Float(T.Runs(I)) - mu) / sigma;
            T.Each_Bit_p_value(I) := Stats.Z_Score(Z);
         else
            -- If a generator has an unchanging bit in its output (e.g. RANDU)
            -- we cannot compute a proper metric
            T.Each_Bit_p_value(I) := -1.0;
         end if;

      end loop;

      T.Ready := True;
   end Compute_Result;

   ------------------
   -- Result_Ready --
   ------------------

   function Result_Ready (T: WW_Runs) return Boolean is (T.Ready);

   ------------
   -- Passed --
   ------------

   function Passed (T : in WW_Runs; p : in Long_Float := 0.01)
                    return Boolean is
     (for all I in 1..T.Width =>
         T.Each_Bit_p_value(I) > Stats.Adjusted_alpha(p, T.Width));

   -------
   -- p --
   -------

   function p (T : in WW_Runs) return Long_Float is
      Result : Long_Float := 0.5;
   begin
      for I in T.Each_Bit_p_value'Range loop
         Result := Long_Float'Min(Result,
                                  Long_Float'Min(T.Each_Bit_p_value(I),
                                    1.0-T.Each_Bit_p_value(I))
                                 );
      end loop;
      return Result;
   end p;

   --------------
   -- Describe --
   --------------

   function Describe_Result (T : in WW_Runs) return String is
      Bits : String(1..T.Width);
      p_value : Long_Float;
      LF : Character renames Ada.Characters.Latin_1.LF;
   begin

      for I in Bits'Range loop
         p_value := T.Each_Bit_p_value(T.Width + 1 - I);
         if p_value = -1.0 then
            Bits(I) := 'z';
         elsif p_value < 1.0E-6 then
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

      return "Wald–Wolfowitz Runs test with width " & Integer'Image(T.Width) & LF &
        "Individual bit Z-score results MSB->LSB, failures ranked a (@ 5%) to d (@ 1E-6) and z (NaN): "
        & LF & Bits;

   end Describe_Result;

end PRNG_Zoo.Tests.Bits;
