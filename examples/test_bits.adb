--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.Register;

with PRNG_Zoo.Tests, PRNG_Zoo.Tests.Bits, PRNG_Zoo.Tests.EquiDist;

with Parse_Args;

with Ada.Text_IO;
use Ada.Text_IO;

procedure test_bits is
   AP : Parse_Args.Argument_Parser;
   Seed : PRNG_Zoo.U64;
   Iterations : Natural;
   X : U64;

begin

   AP.Set_Prologue("Test bit distributions of PRNG.");
   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "help", 'h',
                 Usage => "Display this help text");
   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "list-prng", 'p',
                 Usage => "List available PRNG");
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
   AP.Append_Positional(Parse_Args.Make_String_Option(""), "PRNG");
   AP.Parse_Command_Line;

   if not AP.Parse_Success then
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
      goto Finish;
   elsif AP.Boolean_Value("help") then
      AP.Usage;
      goto Finish;
   elsif AP.Boolean_Value("list-prng") then
      PRNG_Zoo.Register.Display_Register;
      goto Finish;
   elsif AP.String_Value("PRNG") = "" then
      Put_Line("A PRNG must be specified.");
      goto Finish;
   elsif not PRNG_Zoo.Register.PRNG_Exists(AP.String_Value("PRNG")) then
      Put_Line("No such PRNG: '" & AP.String_Value("PRNG") & "'");
      goto Finish;
   end if;

   Seed := PRNG_Zoo.U64(AP.Natural_Value("seed"));
   Iterations := AP.Natural_Value("iterations") * 1_000_000;

   declare
      G : PRNG'Class := Register.Make_PRNG(AP.String_Value("PRNG"));
      BC : Tests.Bits.Bit_Counter(AP.Natural_Value("output-width"));
      ED : Tests.EquiDist.EquiDist(t => AP.Natural_Value("dimensions"),
                                   l => AP.Natural_Value("divisions"),
                                   n => AP.Natural_Value("output-width"));
   begin
      G.Reset(Seed);
      BC.Reset;
      ED.Reset;
      Put_Line("Testing " & AP.String_Value("PRNG"));
      New_Line;

      for I in 1..Iterations loop
         X := G.Generate;
         BC.Feed(X);
         ED.Feed(X);
      end loop;

      BC.Compute_Result;
      Put_Line(AP.String_Value("PRNG") &
               (if BC.Passed then " passed " else " failed " ) &
                 "Bit-Counter test");
      Put_Line(BC.Describe_Result); New_Line;

      ED.Compute_Result;
      Put_Line(AP.String_Value("PRNG") &
               (if ED.Passed then " passed " else " failed " ) &
                 "Equidistribution test");
      Put_Line(ED.Describe_Result); New_Line;

   end;

   <<Finish>>
   null;
end test_bits;
