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

package body PRNG_Zoo.Linear_Congruential is

   -----------------
   -- Generic_LCG --
   -----------------

   package body Generic_LCG is

      -----------
      -- Reset --
      -----------

      procedure Reset (G: in out LCG; S: in U64) is
      begin
         G.s := S;
      end Reset;

      --------------
      -- Generate --
      --------------

      function Generate (G: in out LCG) return U64 is
      begin
         G.s := (G.s * Multiplier + Increment) mod Modulus;
         return G.s;
      end Generate;

   end Generic_LCG;

   ------------------------
   -- Generic_LCG_32Only --
   ------------------------

   package body Generic_LCG_32Only is

      -----------
      -- Reset --
      -----------

      procedure Reset (G: in out LCG_32Only; S: in U64) is
      begin
         G.s := U32(S and 16#FFFFFFFF#);
      end Reset;

      --------------
      -- Generate --
      --------------

      function Generate (G: in out LCG_32Only) return U32 is
      begin
         G.s := (G.s * Multiplier + Increment) mod Modulus;
         return G.s;
      end Generate;

   end Generic_LCG_32Only;

   -----------------
   -- Constructor --
   -----------------

   function Constructor(Params : not null access PRNG_Parameters'Class) return LCG is
      P : LCG_Parameters;
   begin
      if Params.all in LCG_Parameters then
         P := LCG_Parameters(Params.all);
         return LCG'(Modulus      => P.Modulus,
                     Multiplier   => P.Multiplier,
                     Increment    => P.Increment,
                     Usable_Width => P.Usable_Width,
                     s            => P.s);
      elsif Params.Contains("Modulus") and
        Params.Contains("Multiplier") and
        Params.Contains("Increment") and
        Params.Contains("Usable_Width") then
         return LCG'(Modulus      => Params.Parameter("Modulus"),
                     Multiplier   => Params.Parameter("Multiplier"),
                     Increment    => Params.Parameter("Increment"),
                     Usable_Width => Params.Parameter("Usable_Width"),
                     s            => 1);
      else
         raise Invalid_Parameters with "Must specify Modulus, Multiplier, Increment and Usable_Width for LCG generator";
      end if;
   end Constructor;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out LCG; S: in U64) is
   begin
      G.s := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out LCG) return U64 is
   begin
      G.s := (G.s * G.Multiplier + G.Increment) mod G.Modulus;
      return G.s;
   end Generate;

   -----------------
   -- Constructor --
   -----------------

   function Constructor(Params : not null access PRNG_Parameters'Class) return LCG_32Only is
      P : LCG_32Only_Parameters;
   begin
      if Params.all in LCG_32Only_Parameters then
         P := LCG_32Only_Parameters(Params.all);
         return LCG_32Only'(Modulus      => P.Modulus,
                            Multiplier   => P.Multiplier,
                            Increment    => P.Increment,
                            Usable_Width => P.Usable_Width,
                            s            => P.s);
      elsif Params.Contains("Modulus") and
        Params.Contains("Multiplier") and
        Params.Contains("Increment") and
        Params.Contains("Usable_Width") then
         return LCG_32Only'(Modulus      => U32(U64'(Params.Parameter("Modulus"))),
                            Multiplier   => U32(U64'(Params.Parameter("Multiplier"))),
                            Increment    => U32(U64'(Params.Parameter("Increment"))),
                            Usable_Width => Params.Parameter("Usable_Width"),
                            s            => 1);
      else
         raise Invalid_Parameters with "Must specify Modulus, Multiplier, Increment and Usable_Width for LCG32 generator";
      end if;
   end Constructor;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out LCG_32Only; S: in U64) is
   begin
      G.s := U32(S and 16#FFFFFFFF#);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out LCG_32Only) return U32 is
   begin
      G.s := (G.s * G.Multiplier + G.Increment) mod G.Modulus;
      return G.s;
   end Generate;

end PRNG_Zoo.Linear_Congruential;
