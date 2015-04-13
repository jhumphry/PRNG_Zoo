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

with PRNG_Zoo.Tests, PRNG_Zoo.Tests.Bits, PRNG_Zoo.Tests.EquiDist;

with Parse_Args;

with Common_CLI, Common_CLI_Options;
use Common_CLI_Options;

with Ada.Text_IO;
use Ada.Text_IO;

procedure test_bits is
   AP : Parse_Args.Argument_Parser;
   PRNG_Names : Parse_Args.String_Doubly_Linked_Lists.List;
   Seed : PRNG_Zoo.U64;
   Seed_From_Array : U64_array_access;
   Iterations : Natural;
   X : U64;

begin

   AP.Set_Prologue("Test bit distributions of PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(1), "iterations", 'i',
                 Usage => "Specify iterations (in millions) (default 1)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(2), "dimensions", 't',
                 Usage => "Log_{2} Dimensionality of equidistribution test (default 2)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(2), "divisions", 'l',
                 Usage => "Log_{2} Divisions in each dimension (default 2)");

   Common_CLI(AP, PRNG_Names);

   Seed := U64_Options.Value(AP, "seed");
   Seed_From_Array := U64_array_Options.Value(AP, "seed-from-array");
   Iterations := AP.Integer_Value("iterations") * 1_000_000;

   for Name of PRNG_Names loop

      declare
         G : PRNG'Class := Register.Make_PRNG(Name);
         BC : Tests.Bits.Bit_Counter(G.Width);
         WW : Tests.Bits.WW_Runs(G.Width);
         ED : Tests.EquiDist.EquiDist(t => AP.Integer_Value("dimensions"),
                                      l => AP.Integer_Value("divisions"),
                                      n => G.Width);
      begin
         if Seed_From_Array /= null and G in PRNG_Seed_From_Array'Class then
            PRNG_Seed_From_Array'Class(G).Reset(Seed_From_Array.all);
         else
            G.Reset(Seed);
         end if;
         BC.Reset;
         WW.Reset;
         ED.Reset;
         Put_Line("Testing " & Name);
         New_Line;

         for I in 1..Iterations loop
            X := G.Generate;
            BC.Feed(X);
            WW.Feed(X);
            ED.Feed(X);
         end loop;

         BC.Compute_Result;
         Put_Line(Name &
                  (if BC.Passed then " passed " else " failed " ) &
                    "Bit-Counter test:");
         Put_Line(BC.Describe_Result); New_Line;

         WW.Compute_Result;
         Put_Line(Name &
                  (if WW.Passed then " passed " else " failed " ) &
                    "WW Runs test:");
         Put_Line(WW.Describe_Result); New_Line;

         ED.Compute_Result;
         Put_Line(Name &
                  (if ED.Passed then " passed " else " failed " ) &
                    "Equidistribution test:");
         Put_Line(ED.Describe_Result); New_Line;

      end;
      New_Line;
   end loop;

end test_bits;
