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

package body PRNG_Zoo.Tests.EquiDist is

   -----------
   -- Reset --
   -----------

   procedure Reset (T : in out EquiDist) is
      Num_Bins : Natural := 2**(T.t * T.l);
   begin

      if T.l >= T.n then
         raise Constraint_Error
           with "Cannot have finer subdivisions per dimension than the number of input bits";
      end if;

      T.next_dimension := 1;
      T.current := (others => 0);

      if T.bins = null then
         T.bins := new Counter_array(1..Num_Bins);
      end if;

      if T.bins'Length /= Num_Bins then
         raise Constraint_Error
           with "Somehow an incorrect number of bins have been allocated.";
      end if;

      T.bins.all := (others => 0);
      T.chi2_cdf_result := -1.0;
   end Reset;

   ----------
   -- Feed --
   ----------

   procedure Feed (T : in out EquiDist; X : in U64) is
      stride : Positive;
      position : Natural;
   begin
      T.current(T.next_dimension) := Shift_Right((X and T.mask), T.n-T.l);
      if T.next_dimension = T.t then
         stride := 1;
         position := 0;
         for I in T.current'Range loop
            position := position + Natural(T.current(I)) * stride;
            stride := stride * 2 ** T.l;
         end loop;
         T.bins(position+1) := T.bins(position+1) + 1;
         T.next_dimension := 1;
      else
         T.next_dimension := T.next_dimension + 1;
      end if;
   end Feed;

   --------------------
   -- Compute_Result --
   --------------------

   procedure Compute_Result(T : in out EquiDist) is
      N : Positive := 2 ** (T.t * T.l);
      Expected : Long_Float := 1.0 / Long_Float(N);
      B : Binned(N);
   begin
      B.Bin_Counts := T.bins.all;
      B.Bin_Expected := (others => Expected);
      B.Distribution_DF := 1;
      T.chi2_cdf_result := Stats.Chi2_CDF_Bins(B);
   end Compute_Result;

   ------------------
   -- Result_Ready --
   ------------------

   function Result_Ready(T: EquiDist) return Boolean is
   begin
      return T.chi2_cdf_result /= -1.0;
   end Result_Ready;

   ------------
   -- Passed --
   ------------

   function Passed
     (T : in EquiDist;
      p : in Long_Float := 0.01)
      return Boolean
   is
   begin
      return T.chi2_cdf_result > p and T.chi2_cdf_result < (1.0-p);
   end Passed;

   -------
   -- p --
   -------

   function p (T : in EquiDist) return Long_Float is
   begin
      return T.chi2_cdf_result;
   end p;

   ---------------------
   -- Describe_Result --
   ---------------------

   function Describe_Result (T : in EquiDist) return String is
   begin
      return "Chi2 Equidistribution test (" & Integer'Image(T.t) &
        "," & Integer'Image(T.l) & ") with result := " &
        Long_Float'Image(T.chi2_cdf_result);
   end Describe_Result;


   -------------------
   -- Make_EquiDist --
   -------------------

   function Make_EquiDist (t, l: Positive; n : Positive := 64) return EquiDist is
      Num_Bins : Natural := 2**(t * l);
   begin
      if l >= n then
         raise Constraint_Error
           with "Cannot have finer subdivisions per dimension than the number of input bits";
      end if;

      return Result: EquiDist(t, l, n) do
         Result.next_dimension := 1;
         Result.current := (others => 0);
         Result.bins := new Counter_array(1..Num_Bins);
         Result.bins.all := (others => 0);
         Result.chi2_cdf_result := -1.0;
      end return;
   end Make_EquiDist;

end PRNG_Zoo.Tests.EquiDist;
