--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.Register;

with Parse_Args;

with Common_CLI;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Containers;
use type Ada.Containers.Count_Type;

with Ada.Calendar, Ada.Real_Time, Ada.Execution_Time;
use all type Ada.Execution_Time.CPU_Time;
use all type Ada.Real_Time.Time_Span;

procedure speedtest is
   AP : Parse_Args.Argument_Parser;
   PRNG_Names : Parse_Args.String_Doubly_Linked_Lists.List;
   Seed : PRNG_Zoo.U64;
   Iterations : Natural;
   X : U64;

   Names, Descriptions : Natural := 0;

   Start_Time, End_Time : Ada.Execution_Time.CPU_Time;

begin

   AP.Set_Prologue("Test the speed of different PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(9753), "seed", 's',
                 Usage => "Specify a seed for the generators (default 9753)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(1), "iterations", 'i',
                 Usage => "Specify iterations (in millions) (default 1)");

   Common_CLI(AP, PRNG_Names);

   if PRNG_Names.Length = 0 then
      goto Finish;
   end if;

   Seed := PRNG_Zoo.U64(AP.Natural_Value("seed"));
   Iterations := AP.Natural_Value("iterations") * 1_000_000;

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

      begin
         G.Reset(Seed);

         Put(Name);
         Set_Col (Ada.Text_IO.Count(Names + 1));
         Put("| ");

         Start_Time := Ada.Execution_Time.Clock;
         for I in 1..Iterations loop
            X := G.Generate;
         end loop;
         End_Time := Ada.Execution_Time.Clock;
      end;

      Put(Duration'Image(To_Duration(End_Time-Start_Time)));
      New_Line;
   end loop;

   New_Line;

   <<finish>>
   null;
end speedtest;
