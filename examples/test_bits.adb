--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.Register;

with PRNG_Zoo.Tests, PRNG_Zoo.Tests.Bits, PRNG_Zoo.Tests.EquiDist;

with Parse_Args;

with Common_CLI;

with Ada.Text_IO;
use Ada.Text_IO;

procedure test_bits is
   AP : Parse_Args.Argument_Parser;
   PRNG_Names : Parse_Args.String_Doubly_Linked_Lists.List;
   Seed : PRNG_Zoo.U64;
   Iterations : Natural;
   X : U64;

begin

   AP.Set_Prologue("Test bit distributions of PRNG.");

   AP.Add_Option(Parse_Args.Make_Natural_Option(9753), "seed", 's',
                 Usage => "Specify a seed for the generators (default 9753)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(1), "iterations", 'i',
                 Usage => "Specify iterations (in millions) (default 1)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(64), "output-width", 'w',
                 Usage => "Number of bits of output to test (default 64)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(2), "dimensions", 't',
                 Usage => "Log_{2} Dimensionality of equidistribution test (default 2)");
   AP.Add_Option(Parse_Args.Make_Natural_Option(2), "divisions", 'l',
                 Usage => "Log_{2} Divisions in each dimension (default 2)");

   Common_CLI(AP, PRNG_Names);

   Seed := PRNG_Zoo.U64(AP.Natural_Value("seed"));
   Iterations := AP.Natural_Value("iterations") * 1_000_000;

   for Name of PRNG_Names loop

      declare
         G : PRNG'Class := Register.Make_PRNG(Name);
         BC : Tests.Bits.Bit_Counter(AP.Natural_Value("output-width"));
         ED : Tests.EquiDist.EquiDist(t => AP.Natural_Value("dimensions"),
                                      l => AP.Natural_Value("divisions"),
                                      n => AP.Natural_Value("output-width"));
      begin
         G.Reset(Seed);
         BC.Reset;
         ED.Reset;
         Put_Line("Testing " & Name);
         New_Line;

         for I in 1..Iterations loop
            X := G.Generate;
            BC.Feed(X);
            ED.Feed(X);
         end loop;

         BC.Compute_Result;
         Put_Line(Name &
                  (if BC.Passed then " passed " else " failed " ) &
                    "Bit-Counter test:");
         Put_Line(BC.Describe_Result); New_Line;

         ED.Compute_Result;
         Put_Line(Name &
                  (if ED.Passed then " passed " else " failed " ) &
                    "Equidistribution test:");
         Put_Line(ED.Describe_Result); New_Line;

      end;
      New_Line;
   end loop;

end test_bits;
