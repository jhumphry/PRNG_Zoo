--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.Register;

with Parse_Args;
use type Parse_Args.Integer_Array;

with Common_CLI;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Containers;
use type Ada.Containers.Count_Type;

procedure sample_output is
   package U64_IO is new Ada.Text_IO.Modular_IO(Num => U64);
   use U64_IO;

   package U32_IO is new Ada.Text_IO.Modular_IO(Num => U32);
   use U32_IO;

   AP : Parse_Args.Argument_Parser;
   PRNG_Names : Parse_Args.String_Doubly_Linked_Lists.List;
   Seed : PRNG_Zoo.U64;
   Number : Natural;
   Columns : Natural;
   Generate_32bit: Boolean;

begin

   AP.Set_Prologue("Generate samples of the output from different PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(9753), "seed", 's',
                 Usage => "Specify a seed for the generators (default 9753)");
    AP.Add_Option(Parse_Args.Make_Integer_Array_Option, "seed-from-array", 'a',
                 Usage => "Seed using an array of integers, where possible");
   AP.Add_Option(Parse_Args.Make_Natural_Option(16), "number", 'n',
                 Usage => "Specify number of outputs per PRNG (default 16)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(2), "columns", 'c',
                 Usage => "Specify number of output columns (default 2)");
   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "generate-32bit", 'g',
                 Usage => "Generate 32-bit outputs (default false)");

   Common_CLI(AP, PRNG_Names);

   if PRNG_Names.Length = 0 then
      goto Finish;
   end if;

   Seed := PRNG_Zoo.U64(AP.Natural_Value("seed"));
   Number := AP.Natural_Value("number");
   Columns := AP.Natural_Value("columns");
   Generate_32bit := AP.Boolean_Value("generate-32bit");

   for Name of PRNG_Names loop
      declare
         G : PRNG'Class := Register.Make_PRNG(Name);
         Seed_From_Array : Parse_Args.Integer_Array
           := AP.Integer_Array_Value("seed-from-array");
         Seed_Array : U64_Array(Seed_From_Array'Range);
      begin

         Put(Number);
         Put(" outputs from " & Name);

         if Seed_From_Array /= Parse_Args.Empty_Integer_Array
           and G in PRNG_Seed_From_Array'Class then

            for I in Seed_From_Array'Range loop
               Seed_Array(I) := U64(Seed_From_Array(I));
            end loop;

            PRNG_Seed_From_Array'Class(G).Reset(Seed_Array);

            Put(" seeded from an array.");

         else

            G.Reset(Seed);

            Put(" with seed: ");
            Put(Seed);
            Put(".");

         end if;

         New_Line;

         for I in 1..Number loop
            if Generate_32bit then
               Put(U32'(G.Generate));
            else
               Put(G.Generate_Padded);
            end if;
            if I mod Columns = 0  then
               New_Line;
            else
               Put(" ");
            end if;
         end loop;
      end;

      New_Line;
   end loop;

   New_Line;

   <<finish>>
   null;
end sample_output;
