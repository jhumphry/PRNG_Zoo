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

with Ada.Text_IO, Ada.Long_Float_Text_IO;
use Ada.Text_IO,  Ada.Long_Float_Text_IO;

with Ada.Containers;
use all type Ada.Containers.Count_Type;

with Ada.Numerics.Long_Elementary_Functions;

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.Register;

with PRNG_Zoo.Distributions;
with PRNG_Zoo.Tests;
with PRNG_Zoo.Tests.Distributions;

with Parse_Args;

with Common_CLI, Common_CLI_Options;
use Common_CLI_Options;

procedure test_distributions is



   AP : Parse_Args.Argument_Parser;
   PRNG_Names : Parse_Args.String_Doubly_Linked_Lists.List;
   Seed : PRNG_Zoo.U64;
   Seed_From_Array : U64_array_access;
   Iterations : Natural;

begin

   AP.Set_Prologue("Test use of PRNG to generate variates.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(1), "iterations", 'i',
                 Usage => "Specify iterations (in millions) (default 1)");

   Common_CLI(AP, PRNG_Names);

   if PRNG_Names.Length = 0 then
      goto Finish;
   end if;

   Seed := U64_Options.Value(AP, "seed");
   Seed_From_Array := U64_array_Options.Value(AP, "seed-from-array");
   Iterations := AP.Integer_Value("iterations") * 1_000_000;

   Put("Standard error at " & Integer'Image(Iterations) & " iterations: ");
   Put(1.0 / Ada.Numerics.Long_Elementary_Functions.Sqrt(Long_Float(Iterations)));
   New_Line; New_Line;

   for Name of PRNG_Names loop

      declare
         G : PRNG'Class := Register.Make_PRNG(Name);

         package LFD is new Distributions(Float_Type => Long_Float,
                                          scale => (
                                                    case G.Width is
                                                       when 31 => scale_U31,
                                                       when 32 => scale_U32,
                                                       when 64 => scale_U64,
                                                       when others => raise Constraint_Error
                                                   )
                                         );
         use LFD;
         use LFD.EF;
         package LFD_Tests is new PRNG_Zoo.Tests.Distributions(Dist => LFD);

         D_Normal_12_6 : LFD.Normal_12_6;
         D_Normal_BM : LFD.Normal_Box_Mueller;
         D_Normal_MP : LFD.Normal_Monty_Python;
         T_Chi2Normal : LFD_Tests.NormalChi2(200);

      begin
         if Seed_From_Array /= null and G in PRNG_Seed_From_Array'Class then
            PRNG_Seed_From_Array'Class(G).Reset(Seed_From_Array.all);
         else
            G.Reset(Seed);
         end if;
         D_Normal_12_6.Reset;
         T_Chi2Normal.Reset;
         Put_Line("12-6 method with " & Name & " generator:");
         LFD_Tests.Run_Test(G => G,
                            D => D_Normal_12_6,
                            T => T_Chi2Normal,
                            iterations => Iterations);


         Put_Line("Chi2 test: " & (if T_Chi2Normal.Passed then "Passed" else "Failed"));
         Put(T_Chi2Normal.Describe_Result);
         New_Line(2);

         if Seed_From_Array /= null and G in PRNG_Seed_From_Array'Class then
            PRNG_Seed_From_Array'Class(G).Reset(Seed_From_Array.all);
         else
            G.Reset(Seed);
         end if;
         D_Normal_BM.Reset;
         T_Chi2Normal.Reset;
         Put_Line("Box-Mueller method with " & Name & " generator:");
         LFD_Tests.Run_Test(G => G,
                            D => D_Normal_BM,
                            T => T_Chi2Normal,
                            iterations => Iterations);


         Put_Line("Chi2 test: " & (if T_Chi2Normal.Passed then "Passed" else "Failed"));
         Put(T_Chi2Normal.Describe_Result);
         New_Line(2);

         if Seed_From_Array /= null and G in PRNG_Seed_From_Array'Class then
            PRNG_Seed_From_Array'Class(G).Reset(Seed_From_Array.all);
         else
            G.Reset(Seed);
         end if;
         D_Normal_MP.Reset;
         T_Chi2Normal.Reset;
         Put_Line("Monty-Python method with " & Name & " generator:");
         LFD_Tests.Run_Test(G => G,
                            D => D_Normal_MP,
                            T => T_Chi2Normal,
                            iterations => Iterations);


         Put_Line("Chi2 test: " & (if T_Chi2Normal.Passed then "Passed" else "Failed"));
         Put(T_Chi2Normal.Describe_Result);
         New_Line(2);
      end;
   end loop;

   <<Finish>>
   null;

end test_distributions;
