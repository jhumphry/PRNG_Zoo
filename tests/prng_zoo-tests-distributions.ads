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

with PRNG_Zoo.Distributions;

generic
   with package Dist is new PRNG_Zoo.Distributions(<>);
package PRNG_Zoo.Tests.Distributions is

   type Test_Distribution is limited interface and Test;
   procedure Reset(T : in out Test_Distribution) is abstract;
   procedure Feed(T : in out Test_Distribution; X : in Dist.Float_Type) is abstract;
   procedure Compute_Result(T : in out Test_Distribution) is abstract;

   procedure Run_Test(G : in out PRNG'Class;
                      D : in out Dist.Distribution'Class;
                      T : in out Test_Distribution'Class;
                      iterations : Positive := 1_000_000);

   type Test_Distribution_Ptr is access all Test_Distribution'Class;

   -- This test checks a supposed normal variate using N bins
   type NormalChi2(N : Positive) is new Binned and Test_Distribution with private;
   procedure Reset(T : in out NormalChi2);
   procedure Feed(T : in out NormalChi2; X : in Dist.Float_Type) with Inline;
   procedure Compute_Result(T : in out NormalChi2);
   function Result_Ready(T: NormalChi2) return Boolean;
   function Passed(T : in NormalChi2; p : in Long_Float := 0.01) return Boolean;
   function p(T : in NormalChi2) return Long_Float;
   function Describe_Result(T : in NormalChi2) return String;

private

   type NormalChi2(N : Positive) is new Binned(N) and Test_Distribution with
      record
         chi2_value : Long_Float;
         chi2_cdf_result : Long_Float := -1.0;
      end record;

end PRNG_Zoo.Tests.Distributions;
