--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo;
use PRNG_Zoo;

with PRNG_Zoo.xorshift_star, PRNG_Zoo.Linear_Congruential.Examples;
use all type PRNG_Zoo.xorshift_star.xorshift1024_star;
use all type PRNG_Zoo.Linear_Congruential.Examples.RANDU;
use all type PRNG_Zoo.Linear_Congruential.Examples.MINSTD;

with PRNG_Zoo.Tests, PRNG_Zoo.Tests.Bits, PRNG_Zoo.Tests.EquiDist;

with Parse_Args;

with Ada.Text_IO;
use Ada.Text_IO;

procedure test_bits is
   AP : Parse_Args.Argument_Parser;
   Seed : PRNG_Zoo.U64;
   Iterations : Natural;

   G1 : xorshift_star.xorshift1024_star;
   G2 : Linear_Congruential.Examples.RANDU;
   G3 : Linear_Congruential.Examples.MINSTD;
   X : U64;
   BC : Tests.Bits.Bit_Counter(64);
   BC_31 : Tests.Bits.Bit_Counter(31);
   ED_64 : Tests.EquiDist.EquiDist(3,2,64);
   ED_31 : Tests.EquiDist.EquiDist(2,2,31);
begin

   AP.Set_Prologue("A simple demonstration of the Parse_Args library.");
   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "help", 'h',
                 Usage => "Display this help text");
   AP.Add_Option(Parse_Args.Make_Natural_Option(9753), "seed", 's',
                 Usage => "Specify a seed for the generators");
   AP.Add_Option(Parse_Args.Make_Natural_Option(1), "iterations", 'i',
                 Usage => "Specify iterations (in millions)");
   AP.Parse_Command_Line;

   if not AP.Parse_Success then
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
      goto Finish;
   elsif AP.Boolean_Value("help") then
      AP.Usage;
      goto Finish;
   end if;

   Seed := PRNG_Zoo.U64(AP.Natural_Value("seed"));
   Iterations := AP.Natural_Value("iterations") * 1_000_000;

   G1.Reset(Seed);
   BC.Reset; ED_64.Reset;
   Put_Line("Testing xorshift1024*");
   for I in 1..Iterations loop
      X := G1.Generate;
      BC.Feed(X);
      ED_64.Feed(X);
   end loop;
   BC.Compute_Result;
   if BC.Passed then
      Put_Line("xorshift1024* passed Bit-Counter test");
   else
      Put_Line("xorshift1024* failed Bit-Counter test");
   end if;
   Put_Line(BC.Describe_Result); New_Line;

   ED_64.Compute_Result;
   if ED_64.Passed then
      Put_Line("xorshift1024* passed Equidistribution test");
   else
      Put_Line("xorshift1024* failed Equidistribution test");
   end if;

   Put_Line(ED_64.Describe_Result); New_Line;

   G2.Reset(Seed);
   BC_31.Reset; ED_31.Reset;
   Put_Line("Testing RANDU");
   for I in 1..Iterations loop
      X := U64(U32'(G2.Generate));
      BC_31.Feed(X);
      ED_31.Feed(X);
   end loop;
   BC_31.Compute_Result;
   if BC_31.Passed then
      Put_Line("RANDU passed Bit-Counter test");
   else
      Put_Line("RANDU failed Bit-Counter test");
   end if;
   Put_Line(BC_31.Describe_Result); New_Line;

   ED_31.Compute_Result;
   if ED_31.Passed then
      Put_Line("RANDU passed Equidistribution test");
   else
      Put_Line("RANDU failed Equidistribution test");
   end if;

   Put_Line(ED_31.Describe_Result); New_Line;

   G3.Reset(Seed);
   BC_31.Reset; ED_31.Reset;
   Put_Line("Testing MINSTD");
   for I in 1..Iterations loop
      X := U64(U32'(G3.Generate));
      BC_31.Feed(X);
      ED_31.Feed(X);
   end loop;
   BC_31.Compute_Result;
   if BC_31.Passed then
      Put_Line("MINSTD passed Bit-Counter test");
   else
      Put_Line("MINSTD failed Bit-Counter test");
   end if;
   Put_Line(BC_31.Describe_Result); New_Line;

   ED_31.Compute_Result;
   if ED_31.Passed then
      Put_Line("MINSTD passed Equidistribution test");
   else
      Put_Line("MINSTD failed Equidistribution test");
   end if;

   Put_Line(ED_31.Describe_Result); New_Line;

   <<Finish>>
   null;
end test_bits;
