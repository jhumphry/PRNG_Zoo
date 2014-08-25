--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.xorshift_star;
use PRNG_Zoo;
use all type PRNG_Zoo.xorshift_star.xorshift1024_star;

with PRNG_Zoo.Distributions;

with Ada.Text_IO, Ada.Long_Float_Text_IO;
use Ada.Text_IO,  Ada.Long_Float_Text_IO;

procedure test_distributions is
   package LFD is new PRNG_Zoo.Distributions(Float_Type => Long_Float,
                                             P => xorshift_star.xorshift1024_star);
   use LFD;
   G : xorshift_star.xorshift1024_star;
   D_Uniform01 : LFD.Uniform01;
   D_Normal_BM : LFD.Normal_Box_Mueller;
begin

   Put_Line("10 uniform [0,1] variates from seed 5489.");
   G.Reset(5489);
   for I in 1..10 loop
      Put(Integer'Image(I) & ":");
      Put(D_Uniform01.Generate(G));
      New_Line;
   end loop;

   Put_Line("10 Box-Mueller normal variates from seed 5489.");
   G.Reset(5489);
   for I in 1..10 loop
      Put(Integer'Image(I) & ":");
      Put(D_Normal_BM.Generate(G));
      New_Line;
   end loop;

end test_distributions;
