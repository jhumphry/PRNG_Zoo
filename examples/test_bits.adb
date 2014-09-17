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

with Ada.Text_IO;
use Ada.Text_IO;

procedure test_bits is
   G1 : xorshift_star.xorshift1024_star;
   G2 : Linear_Congruential.Examples.RANDU;
   G3 : Linear_Congruential.Examples.MINSTD;
   X : U64;
   BC : Tests.Bits.Bit_Counter(64);
   BC_31 : Tests.Bits.Bit_Counter(31);
   ED_64 : Tests.EquiDist.EquiDist(3,2,64);
   ED_31 : Tests.EquiDist.EquiDist(2,2,31);
begin

   G1.Reset(9753);
   BC.Reset; ED_64.Reset;
   Put_Line("Testing xorshift1024*");
   for I in 1..1_000_000 loop
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

   G2.Reset(9753);
   BC_31.Reset; ED_31.Reset;
   Put_Line("Testing RANDU");
   for I in 1..1_000_000 loop
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

   G3.Reset(9753);
   BC_31.Reset; ED_31.Reset;
   Put_Line("Testing MINSTD");
   for I in 1..1_000_000 loop
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

end test_bits;
