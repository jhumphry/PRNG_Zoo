--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.xorshift_star, PRNG_Zoo.MT;
use PRNG_Zoo;
use all type PRNG_Zoo.U64;
use all type PRNG_Zoo.U32;

use all type PRNG_Zoo.xorshift_star.xorshift1024_star;
use all type PRNG_Zoo.MT.MT19937;

with PRNG_Zoo.Distributions;
with PRNG_Zoo.Tests;
with PRNG_Zoo.Tests.Distributions;

with Ada.Text_IO, Ada.Long_Float_Text_IO;
use Ada.Text_IO,  Ada.Long_Float_Text_IO;

procedure test_distributions is

   package LFD is new Distributions(Float_Type => Long_Float,
                                    P => xorshift_star.xorshift1024_star,
                                    Mod_Type => PRNG_Zoo.U64,
                                    scale => scale_U64);
   use LFD;
   use LFD.EF;
   package LFD_Tests is new PRNG_Zoo.Tests.Distributions(Dist => LFD);


   package LFD_32 is new Distributions(Float_Type => Long_Float,
                                    P => MT.MT19937,
                                    Mod_Type => PRNG_Zoo.U32,
                                    scale => scale_U32);
   use LFD_32;
   package LFD_32_Tests is new PRNG_Zoo.Tests.Distributions(Dist => LFD_32);

   iterations : constant Integer := 1_000_000;

   G : xorshift_star.xorshift1024_star;
   D_Normal_12_6 : LFD.Normal_12_6;
   D_Normal_BM : LFD.Normal_Box_Mueller;
   D_Normal_MP : LFD.Normal_Monty_Python;
   T_Chi2Normal : LFD_Tests.NormalChi2(200);

   G_32: MT.MT19937;
   D_Normal_MP_32 : LFD_32.Normal_Monty_Python;
   T_Chi2Normal_32 : LFD_32_Tests.NormalChi2(200);

begin

   Put("Standard error at " & Integer'Image(iterations) & " iterations: ");
   Put(1.0 / sqrt(Long_Float(iterations))); New_Line; New_Line;

   G.Reset(5489);
   D_Normal_12_6.Reset;
   T_Chi2Normal.Reset;
   Put_Line("12-6 method with xorshift1024* generator:");

   LFD_Tests.Run_Test(G => G,
                      D => D_Normal_12_6,
                      T => T_Chi2Normal,
                      iterations => iterations);

   Put_Line("Chi2 test: " & (if T_Chi2Normal.Passed then "Passed" else "Failed"));
   Put(T_Chi2Normal.Describe_Result);
   New_Line(2);

   G.Reset(5489);
   D_Normal_BM.Reset;
   T_Chi2Normal.Reset;
   Put_Line("Box-Mueller method with xorshift1024* generator:");

   LFD_Tests.Run_Test(G => G,
                      D => D_Normal_BM,
                      T => T_Chi2Normal,
                      iterations => iterations);

   Put_Line("Chi2 test: " & (if T_Chi2Normal.Passed then "Passed" else "Failed"));
   Put(T_Chi2Normal.Describe_Result);
   New_Line(2);

   G.Reset(5489);
   D_Normal_MP.Reset;
   T_Chi2Normal.Reset;
   Put_Line("Monty-Python method with xorshift1024* generator:");

   LFD_Tests.Run_Test(G => G,
                      D => D_Normal_MP,
                      T => T_Chi2Normal,
                      iterations => iterations);

   Put_Line("Chi2 test: " & (if T_Chi2Normal.Passed then "Passed" else "Failed"));
   Put(T_Chi2Normal.Describe_Result);
   New_Line(2);

   G_32.Reset(5489);
   D_Normal_MP_32.Reset;
   T_Chi2Normal_32.Reset;
   Put_Line("Monty-Python method with 32-bit MT19937 generator:");

   LFD_32_Tests.Run_Test(G => G_32,
                         D => D_Normal_MP_32,
                         T => T_Chi2Normal_32,
                         iterations => iterations);

   Put_Line("Chi2 test: " & (if T_Chi2Normal_32.Passed then "Passed" else "Failed"));
   Put(T_Chi2Normal_32.Describe_Result);
   New_Line(2);

end test_distributions;
