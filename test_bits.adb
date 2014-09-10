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
   BC : Tests.Bits.Bit_Counter;
   ED : Tests.Test_Ptr := Tests.EquiDist.Make_EquiDist(3, 3);
   TR : Tests.Test_Result_Ptr;
begin

   G1.Reset(9753);
   BC.Reset; ED.Reset;
   Put_Line("Testing xorshift1024*");
   for I in 1..10_000_000 loop
      X := G1.Generate;
      BC.Feed(X);
      ED.Feed(X);
   end loop;
   TR := BC.Result;
   if TR.Passed then
      Put_Line("xorshift1024* passed Bit-Counter test");
   else
      Put_Line("xorshift1024* failed Bit-Counter test");
   end if;
   Put_Line(TR.Describe); New_Line;

   TR := ED.Result;
   if TR.Passed then
      Put_Line("xorshift1024* passed Equidistribution test");
   else
      Put_Line("xorshift1024* failed Equidistribution test");
   end if;

   Put_Line(TR.Describe); New_Line;

   G2.Reset(9753);
   BC.Reset;
   Put_Line("Testing RANDU");
   for I in 1..1_000_000 loop
      BC.Feed(U64(U32'(G2.Generate)));
   end loop;
   TR := BC.Result(Width => 31);
   if TR.Passed then
      Put_Line("RANDU passed Bit-Counter test");
   else
      Put_Line("RANDU failed Bit-Counter test");
   end if;
   Put_Line(TR.Describe); New_Line;

   G3.Reset(9753);
   BC.Reset;
   Put_Line("Testing MINSTD");
   for I in 1..1_000_000 loop
      BC.Feed(U64(U32'(G3.Generate)));
   end loop;
   TR := BC.Result(Width => 31);
   if TR.Passed then
      Put_Line("MINSTD passed Bit-Counter test");
   else
      Put_Line("MINSTD failed Bit-Counter test");
   end if;
   Put_Line(TR.Describe); New_Line;

end test_bits;
