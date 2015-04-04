--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;
use all type PRNG_Zoo.U64;

with PRNG_Zoo.Register;

with Parse_Args;

with Common_CLI;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Containers;
use type Ada.Containers.Count_Type;

with Ada.Containers.Generic_Array_Sort;

with Ada.Calendar, Ada.Real_Time, Ada.Execution_Time;
use all type Ada.Execution_Time.CPU_Time;
use all type Ada.Real_Time.Time_Span;

procedure speedtest is
   type Duration_Array is array (Integer range <>) of Duration;

   procedure Duration_Array_Sort is new
     Ada.Containers.Generic_Array_Sort(Index_Type   => Integer,
                                       Element_Type => Duration,
                                       Array_Type   => Duration_Array);

   AP : Parse_Args.Argument_Parser;
   PRNG_Names : Parse_Args.String_Doubly_Linked_Lists.List;

   Seed : PRNG_Zoo.U64;
   Iterations, Subsets, Iterations_Per_Subset, Tolerance, Attempts : Natural;
   Names, Descriptions : Natural := 0;

begin

   AP.Set_Prologue("Test the speed of different PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(9753), "seed", 's',
                 Usage => "Specify a seed for the generators (default 9753)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(4), "iterations", 'i',
                 Usage => "Specify iterations (in millions) (default 4)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(8), "sub-sets", 'j',
                 Usage => "Specify number of sub-sets to split iterations into (default 8)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(5), "tolerance", 't',
                 Usage => "Specify tolerance % between median sub-set and min/max subset results (default 5%)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(4), "attempts", 'a',
                 Usage => "Specify number of attempts to make to get subsets results within tolerance (default 4)");

   Common_CLI(AP, PRNG_Names);

   if PRNG_Names.Length = 0 then
      goto Finish;
   end if;

   Seed := PRNG_Zoo.U64(AP.Natural_Value("seed"));
   Iterations := AP.Natural_Value("iterations") * 1_000_000;
   Subsets := AP.Natural_Value("sub-sets");
   Iterations_Per_Subset := Iterations / Subsets;
   Tolerance := AP.Natural_Value("tolerance");
   Attempts := AP.Natural_Value("attempts");

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

   for Name of PRNG_Names loop

      declare
         G : PRNG'Class := Register.Make_PRNG(Name);

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
         G.Reset(Seed);

         Put(Name);
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
              Timings(Subsets) < Median_Per_Subset * (100 + Tolerance) / 100 then
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

   <<finish>>
   null;
end speedtest;
