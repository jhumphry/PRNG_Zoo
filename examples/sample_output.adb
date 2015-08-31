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

with PRNG_Zoo.Register;

with Parse_Args;

with Common_CLI, Common_CLI_Options;
use Common_CLI_Options;

with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;

with Ada.Containers;
use type Ada.Containers.Count_Type;

procedure sample_output is
   package U64_IO is new Ada.Text_IO.Modular_IO(Num => U64);
   use U64_IO;

   package U32_IO is new Ada.Text_IO.Modular_IO(Num => U32);
   use U32_IO;

   AP : Parse_Args.Argument_Parser;
   PRNG_Specs : Common_CLI_Options.PRNG_Spec_Lists.List;
   Seed : PRNG_Zoo.U64;
   Seed_From_Array : U64_array_access;
   Number : Natural;
   Columns : Natural;
   Generate_32bit: Boolean;

begin

   AP.Set_Prologue("Generate samples of the output from different PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(16), "number", 'n',
                 Usage => "Specify number of outputs per PRNG (default 16)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(2), "columns", 'c',
                 Usage => "Specify number of output columns (default 2)");
   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "generate-32bit", 'g',
                 Usage => "Generate 32-bit outputs (default false)");

   Common_CLI(AP, PRNG_Specs);

   if PRNG_Specs.Length = 0 then
      goto Finish;
   end if;

   Seed := U64_Options.Value(AP, "seed");
   Seed_From_Array := U64_array_Options.Value(AP, "seed-from-array");
   Number := AP.Integer_Value("number");
   Columns := AP.Integer_Value("columns");
   Generate_32bit := AP.Boolean_Value("generate-32bit");

   for Spec of PRNG_Specs loop
      declare
         G : PRNG'Class := Register.Make_PRNG(Spec);
      begin

         Put(Number);
         Put(" outputs from " & Register.Name(Spec));

         if Seed_From_Array /= null
           and G in PRNG_Seed_From_Array'Class
         then

            PRNG_Seed_From_Array'Class(G).Reset(Seed_From_Array.all);
            Put(" seeded from an array length: "
                & Integer'Image(Seed_From_Array.all'Length)
                & "."
               );

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

   <<Finish>>
   null;
end sample_output;
