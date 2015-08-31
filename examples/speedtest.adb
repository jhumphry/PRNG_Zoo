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

with PRNG_Zoo;
use PRNG_Zoo;
use all type PRNG_Zoo.U64;

with PRNG_Zoo.Register;

with Parse_Args;

with Common_CLI, Common_CLI_Options;
use Common_CLI_Options;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Containers;
use type Ada.Containers.Count_Type;

with Ada.Containers.Generic_Array_Sort;

with Ada.Real_Time, Ada.Execution_Time;
use all type Ada.Execution_Time.CPU_Time;
use all type Ada.Real_Time.Time_Span;

procedure speedtest is
   type Duration_Array is array (Integer range <>) of Duration;

   procedure Duration_Array_Sort is new
     Ada.Containers.Generic_Array_Sort(Index_Type   => Integer,
                                       Element_Type => Duration,
                                       Array_Type   => Duration_Array);

   AP : Parse_Args.Argument_Parser;
   PRNG_Specs : Common_CLI_Options.PRNG_Spec_Lists.List;

   Seed : PRNG_Zoo.U64;
   Seed_From_Array : U64_array_access;
   Iterations, Subsets, Iterations_Per_Subset, Tolerance, Attempts : Natural;
   Names, Descriptions : Natural := 0;

begin

   AP.Set_Prologue("Test the speed of different PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(4), "iterations", 'i',
                 Usage => "Specify iterations (in millions) (default 4)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(8), "sub-sets", 'j',
                 Usage => "Specify number of sub-sets to split iterations into (default 8)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(5), "tolerance", 't',
                 Usage => "Specify tolerance % between median sub-set and min/max subset results (default 5%)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(4), "attempts", 'u',
                 Usage => "Specify number of attempts to make to get subsets results within tolerance (default 4)");

   Common_CLI(AP, PRNG_Specs);

   if PRNG_Specs.Length = 0 then
      goto Finish;
   end if;

   Seed := U64_Options.Value(AP, "seed");
   Seed_From_Array := U64_array_Options.Value(AP, "seed-from-array");
   Iterations := AP.Integer_Value("iterations") * 1_000_000;
   Subsets := AP.Integer_Value("sub-sets");
   Iterations_Per_Subset := Iterations / Subsets;
   Tolerance := AP.Integer_Value("tolerance");
   Attempts := AP.Integer_Value("attempts");

   PRNG_Zoo.Register.PRNG_Column_Widths(Names, Descriptions);
   Put ("PRNG");
   Set_Col (Ada.Text_IO.Count(Names + 1));
   Put ("| Duration (");
   Put (Natural'Image(Iterations));
   Put(" iterations)");
   New_Line;
   Put(Names * "-");
   Put("+");
   Put(13 * "-");
   New_Line;

   for Spec of PRNG_Specs loop

      declare
         G : PRNG'Class := Register.Make_PRNG(Spec);

         Timings : Duration_Array(1..Subsets);
         Median_Per_Subset : Duration;

         function Run_Subset return Duration is
            X, Y : U64 := 0;
            Start_Time, End_Time : Ada.Execution_Time.CPU_Time;
         begin
            Start_Time := Ada.Execution_Time.Clock;
            for J in 1..Iterations_Per_Subset loop
               X := G.Generate_Padded;
               Y := Y + X;
            end loop;
            End_Time := Ada.Execution_Time.Clock;
            return To_Duration(End_Time-Start_Time);
         end Run_Subset;

      begin
         if Seed_From_Array /= null and G in PRNG_Seed_From_Array'Class then
            PRNG_Seed_From_Array'Class(G).Reset(Seed_From_Array.all);
         else
            G.Reset(Seed);
         end if;

         Put(Register.Name(Spec));
         Set_Col (Ada.Text_IO.Count(Names + 1));
         Put("| ");

         for A in 1..Attempts loop
            for I in 1..Subsets loop
               Timings(I) := Run_Subset;
            end loop;

            Duration_Array_Sort(Timings);

            if Subsets mod 2 = 0 then
               Median_Per_Subset := (Timings(Subsets / 2) + Timings(Subsets / 2 + 1))/2;
            else
               Median_Per_Subset := Timings(Subsets / 2 + 1);
            end if;

            if Timings(1) > Median_Per_Subset * (100 - Tolerance) / 100 and
              Timings(Subsets) < Median_Per_Subset * (100 + Tolerance) / 100
            then
               Put(Duration'Image(Median_Per_Subset * Subsets));
               Put(" " & Duration'Image(Timings(1) * Subsets));
               Put(" - " & Duration'Image(Timings(Subsets) * Subsets));
               exit;
            end if;

         end loop;

         New_Line;
      end;

   end loop;

   New_Line;

   <<Finish>>
   null;
end speedtest;
