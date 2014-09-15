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
with PRNG_Zoo.Stats;
with PRNG_Zoo.Tests.NormalDist;

with Ada.Text_IO, Ada.Long_Float_Text_IO;
use Ada.Text_IO,  Ada.Long_Float_Text_IO;

procedure test_distributions is
   package LFD is new Distributions(Float_Type => Long_Float,
                                    P => xorshift_star.xorshift1024_star,
                                    Mod_Type => PRNG_Zoo.U64,
                                    scale => scale_U64);
   use LFD;
   use LFD.EF;

   procedure sample_normal(G : in out xorshift_star.xorshift1024_star;
                           D : in out LFD.Normal_Distribution'Class;
                           N : Positive := 200;
                           iterations : Positive := 1_000_000)  is
      x, sx, sxx : Long_Float;
      NT : Tests.NormalDist.NormalTest(N);
      TR : Tests.Test_Result_Ptr;
   begin
      NT.Reset;
      sx := 0.0;
      sxx := 0.0;
      for I in 1..iterations loop
         x := D.Generate(G);
         NT.Feed(x);
         sx := sx + x;
         sxx := sxx + x*x;
      end loop;
      Put(Integer'Image(iterations) & " iterations. Mean:");
      Put(sx/Long_Float(iterations)); New_line;
      Put("Variance: ");
      Put((sxx-sx*sx/Long_Float(iterations))/Long_Float(iterations-1)); New_line;
      Put("2-tailed Z-score result: ");
      Put(Stats.Z_Score(sx/Long_Float(iterations)*sqrt(Long_Float(iterations)), True)); New_line;

      TR := NT.Result;
      Put_Line(Tr.Describe);
      Put_Line("Chi2 test for normal distribution " &
               (if TR.Passed then "passed" else "failed."));
      New_Line;
   end sample_normal;

   package LFD_32 is new Distributions(Float_Type => Long_Float,
                                    P => MT.MT19937,
                                    Mod_Type => PRNG_Zoo.U32,
                                    scale => scale_U32);
   use LFD_32;

   procedure sample_normal_32(G : in out MT.MT19937;
                           D : in out LFD_32.Normal_Distribution'Class;
                           N : Positive := 200;
                           iterations : Positive := 1_000_000)  is
      x, sx, sxx : Long_Float;
      NT : Tests.NormalDist.NormalTest(N);
      TR : Tests.Test_Result_Ptr;
   begin
      NT.Reset;
      sx := 0.0;
      sxx := 0.0;
      for I in 1..iterations loop
         x := D.Generate(G);
         NT.Feed(x);
         sx := sx + x;
         sxx := sxx + x*x;
      end loop;
      Put(Integer'Image(iterations) & " iterations. Mean:");
      Put(sx/Long_Float(iterations)); New_line;
      Put("Variance: ");
      Put((sxx-sx*sx/Long_Float(iterations))/Long_Float(iterations-1)); New_line;
      Put("2-tailed Z-score result: ");
      Put(Stats.Z_Score(sx/Long_Float(iterations)*sqrt(Long_Float(iterations)), True)); New_line;

      TR := NT.Result;
      Put_Line(Tr.Describe);
      Put_Line("Chi2 test for normal distribution " &
               (if TR.Passed then "passed" else "failed."));
      New_Line;
   end sample_normal_32;



   iterations : constant Integer := 1_000_000;
   G : xorshift_star.xorshift1024_star;
   G_32: MT.MT19937;
   D_Normal_12_6 : LFD.Normal_12_6;
   D_Normal_BM : LFD.Normal_Box_Mueller;
   D_Normal_MP : LFD.Normal_Monty_Python;
   D_Normal_MP_32 : LFD_32.Normal_Monty_Python;

begin

   Put("Standard error at " & Integer'Image(iterations) & " iterations: ");
   Put(1.0 / sqrt(Long_Float(iterations))); New_Line; New_Line;

   G.Reset(5489);
   Put_Line("12-6 method with xorshift1024* generator:");
   sample_normal(G, D_Normal_12_6, iterations => iterations);

   G.Reset(5489);
   Put_Line("Box-Mueller method with xorshift1024* generator:");
   sample_normal(G, D_Normal_BM, iterations => iterations);

   G.Reset(5489);
   Put_Line("Monty-Python method with xorshift1024* generator:");
   sample_normal(G, D_Normal_MP, iterations => iterations);

   G_32.Reset(5489);
   Put_Line("Monty-Python method with 32-bit MT19937 generator:");
   sample_normal_32(G_32, D_Normal_MP_32, iterations => iterations);

end test_distributions;
