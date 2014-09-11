--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with PRNG_Zoo, PRNG_Zoo.xorshift_star, PRNG_Zoo.MT;
use PRNG_Zoo;
use all type PRNG_Zoo.xorshift_star.xorshift1024_star;
use all type PRNG_Zoo.MT.MT19937_64;

with PRNG_Zoo.Distributions;
with PRNG_Zoo.Stats;

with Ada.Text_IO, Ada.Long_Float_Text_IO;
use Ada.Text_IO,  Ada.Long_Float_Text_IO;

procedure test_distributions is
   package LFD is new Distributions(Float_Type => Long_Float,
                                    P => xorshift_star.xorshift1024_star);
   use LFD;
   use LFD.EF;
   G : xorshift_star.xorshift1024_star;
   D_Normal_12_6 : LFD.Normal_12_6;
   D_Normal_BM : LFD.Normal_Box_Mueller;
   D_Normal_MP : LFD.Normal_Monty_Python;
   iterations : constant := 10_000_000;
   x, sx, sxx : Long_Float;
begin

   Put("Standard error at " & Integer'Image(iterations) & " iterations: ");
   Put(1.0 / sqrt(Long_Float(iterations))); New_Line; New_Line;

   G.Reset(5489);
   sx := 0.0;
   sxx := 0.0;
   for I in 1..iterations loop
      x := D_Normal_12_6.Generate(G);
      sx := sx + x;
      sxx := sxx + x*x;
   end loop;
   Put_Line(Integer'Image(iterations) & " iterations of the 12-6 method for normal variates.");
   Put("Mean: ");
   Put(sx/Long_Float(iterations)); New_line;
   Put("Variance: ");
   Put((sxx-sx*sx/Long_Float(iterations))/Long_Float(iterations-1)); New_line;
   Put("2-tailed Z-score result: ");
   Put(Stats.Z_Score(sx/Long_Float(iterations)*sqrt(Long_Float(iterations)), True)); New_line;
   New_Line;

   G.Reset(5489);
   sx := 0.0;
   sxx := 0.0;
   for I in 1..iterations loop
      x := D_Normal_BM.Generate(G);
      sx := sx + x;
      sxx := sxx + x*x;
   end loop;
   Put_Line(Integer'Image(iterations) & " iterations of Box-Mueller method for normal variates.");
   Put("Mean: ");
   Put(sx/Long_Float(iterations)); New_line;
   Put("Variance: ");
   Put((sxx-sx*sx/Long_Float(iterations))/Long_Float(iterations-1)); New_line;
   Put("2-tailed Z-score result: ");
   Put(Stats.Z_Score(sx/Long_Float(iterations)*sqrt(Long_Float(iterations)), True)); New_line;
   New_Line;

   G.Reset(5489);
   sx := 0.0;
   sxx := 0.0;
   for I in 1..iterations loop
      x := D_Normal_MP.Generate(G);
      sx := sx + x;
      sxx := sxx + x*x;
   end loop;
   Put_Line(Integer'Image(iterations) & " iterations of Monty-Python method for normal variates.");
   Put("Mean: ");
   Put(sx/Long_Float(iterations)); New_line;
   Put("Variance: ");
   Put((sxx-sx*sx/Long_Float(iterations))/Long_Float(iterations-1)); New_line;
   Put("2-tailed Z-score result: ");
   Put(Stats.Z_Score(sx/Long_Float(iterations)*sqrt(Long_Float(iterations)), True)); New_line;
   New_Line;

end test_distributions;
