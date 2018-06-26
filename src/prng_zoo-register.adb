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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with PRNG_Zoo.Filters;
with PRNG_Zoo.LFib;
with PRNG_Zoo.Linear_Congruential, PRNG_Zoo.Linear_Congruential.Examples;
with PRNG_Zoo.Misc;
with PRNG_Zoo.MT;
with PRNG_Zoo.xoroshiro;
with PRNG_Zoo.xorshift;
with PRNG_Zoo.xorshift_plus;
with PRNG_Zoo.xorshift_star;
with PRNG_Zoo.xoshiro;

package body PRNG_Zoo.Register is

   function Contains(Container : PRNG_Parameters_Map; Key : String) return Boolean is
     (Parameters_Maps.Contains(Container.Params, Key));

   function Parameter(Container : PRNG_Parameters_Map; Key : String) return String is
     (Parameters_Maps.Element(Container.Params, Key));

   function Parameter(Container : PRNG_Parameters_Map; Key : String) return U64 is
      S : constant String := Parameters_Maps.Element(Container.Params, Key);
   begin
      begin
         return U64'Value(S);
      exception
         when Constraint_Error =>
            raise Invalid_Parameters with "Value: " & S & " is not a valid unsigned 64-bit value";
      end;
   end Parameter;

   function Parameter(Container : PRNG_Parameters_Map; Key : String) return Integer is
      S : constant String := Parameters_Maps.Element(Container.Params, Key);
   begin
      begin
         return Integer'Value(S);
      exception
         when Constraint_Error =>
            raise Invalid_Parameters with "Value: " & S & " is not a valid Integer value";
      end;
   end Parameter;

   function Split_Parameters(S : String) return PRNG_Parameters_Map is
      Result : PRNG_Parameters_Map;
      U, V, W : Natural;
   begin

      U := S'First;
      for I in 0..Ada.Strings.Fixed.Count(S, ",") loop
         V := Index (Source => S,
                     Pattern => ",",
                     From => U);
         V := (if V = 0 then S'Last+1 else V);
         W := Index (Source => S(U..(V-1)),
                     Pattern => "=");
         if W = 0 then
            raise Constraint_Error with
              "Parameters should be in the form name=value: " & S(U..(V-1));
         end if;
         Result.Params.Insert(S(U..W-1), S(W+1..V-1));
         U := V + 1;
      end loop;

      return Result;
   end Split_Parameters;

   package LFIB_107_378 is new LFIB.Generic_LFib(107,378,"+");

   procedure PRNG_Column_Widths(Names, Descriptions : in out Natural) is
   begin
      Names := 0;
      Descriptions := 0;
      for I in Register.Iterate loop
         if PRNG_Registries.Key(I)'Length > Names then
            Names := PRNG_Registries.Key(I)'Length;
         end if;
         if Length(PRNG_Registries.Element (I).Description) > Descriptions then
            Descriptions := Length(PRNG_Registries.Element (I).Description);
         end if;
      end loop;
   end PRNG_Column_Widths;

   function Make_PRNG(Spec : PRNG_Spec) return PRNG'Class is
      PRNG_Name : constant String := To_String(Spec.Name);
   begin
      if Length(Spec.Params) = 0 then
         return PRNG_Constructor(Register(PRNG_Name).Tag,
                                 Register(PRNG_Name).Params);
      else
         return PRNG_Constructor(Register(PRNG_Name).Tag,
                                 new PRNG_Parameters_Map'(Split_Parameters(To_String(Spec.Params))));
      end if;
   end Make_PRNG;

   procedure Display_Register is
      Names, Descriptions : Natural := 0;
   begin
      PRNG_Column_Widths(Names, Descriptions);

      Put ("PRNG"); Set_Col (Ada.Text_IO.Count(Names + 1)); Put ("| Description");
      New_Line;
      Put(Names * "-");
      Put("+");
      Put((Descriptions+1) * "-");
      New_Line;

      for I in Register.Iterate loop
         Put (PRNG_Registries.Key (I));
         Set_Col (Ada.Text_IO.Count(Names + 1));
         Put ("| " & To_String (PRNG_Registries.Element (I).Description));
         New_Line;
      end loop;
   end Display_Register;

begin

   Register.Insert
     ("Incrementer", PRNG_Details'
        (Tag         => Filters.Incrementer'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Dummy generator: Output is incremented by param incr each time")));

   Register.Insert
     ("LFib_107_378", PRNG_Details'
        (Tag         => LFIB_107_378.LFib'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Additive Lagged Fibbonacci S_n = S_{n-107} + S_{n-378}")));

   Register.Insert
     ("LCG", PRNG_Details'
        (Tag         => Linear_Congruential.LCG'Tag,
         Params_Req  => True,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Generic 64-bit Linear Conguential Generator")));

   Register.Insert
     ("LCG32", PRNG_Details'
        (Tag         => Linear_Congruential.LCG_32Only'Tag,
         Params_Req  => True,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Generic 32-bit Linear Conguential Generator")));

   Register.Insert
     ("MINSTD", PRNG_Details'
        (Tag         => Linear_Congruential.LCG_32Only'Tag,
         Params_Req  => False,
         Params      => Linear_Congruential.Examples.MINSTD0_Parameters'Access,
         Description => To_Unbounded_String ("MINSTD Linear Congruential generator (as per C++ spec)")));

   Register.Insert
     ("MINSTD0", PRNG_Details'
        (Tag         => Linear_Congruential.LCG_32Only'Tag,
         Params_Req  => False,
         Params      => Linear_Congruential.Examples.MINSTD0_Parameters'Access,
         Description => To_Unbounded_String ("MINSTD0, a slightly older varient of MINSTD")));

   Register.Insert
     ("RANDU", PRNG_Details'
        (Tag         => Linear_Congruential.LCG_32Only'Tag,
         Params_Req  => False,
         Params      => Linear_Congruential.Examples.RANDU_Parameters'Access,
         Description => To_Unbounded_String ("RANDU, a very poor but traditional LCG PRNG")));

   Register.Insert
     ("glibc", PRNG_Details'
        (Tag         => Misc.glibc_random'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("GLibc random() generator")));

   Register.Insert
     ("KISS", PRNG_Details'
        (Tag         => Misc.KISS'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Marsaglia's KISS generator")));

   Register.Insert
     ("Murmurhash3", PRNG_Details'
        (Tag         => Misc.MurmurHash3'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Generator based on Murmurhash3")));

   Register.Insert
     ("SplitMix", PRNG_Details'
        (Tag         => Misc.SplitMix'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("SplitMix generator")));

   Register.Insert
     ("mt", PRNG_Details'
        (Tag         => MT.MT19937'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Mersenne Twister (mt19937)")));

   Register.Insert
     ("mt64", PRNG_Details'
        (Tag         => MT.MT19937_64'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("64-bit varient of the Mersenne Twister (mt19937_64)")));

   Register.Insert
     ("TinyMT64", PRNG_Details'
        (Tag         => MT.TinyMT_64'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("64-bit varient of the TinyMT generator")));

   Register.Insert
     ("SHR3", PRNG_Details'
        (Tag         => xorshift.SHR3'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Marsaglia's SHR3 xorshift generator")));

   Register.Insert
     ("xor64", PRNG_Details'
        (Tag         => xorshift.xor64'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("Marsaglia's xor64 xorshift generator")));

   Register.Insert
     ("xoroshiro128+", PRNG_Details'
        (Tag         => xoroshiro.xoroshiro128_plus'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xoroshiro128+ generator")));

   Register.Insert
     ("xoroshiro128**", PRNG_Details'
        (Tag         => xoroshiro.xoroshiro128_star_star'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xoroshiro128** generator")));

   Register.Insert
     ("xorshift128+", PRNG_Details'
        (Tag         => xorshift_plus.xorshift128_plus'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xorshift128+ generator")));

   Register.Insert
     ("xorshift64*", PRNG_Details'
        (Tag         => xorshift_star.xorshift64_star'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xorshift64* generator")));

   Register.Insert
     ("xorshift1024*", PRNG_Details'
        (Tag         => xorshift_star.xorshift1024_star'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xorshift1024* generator")));

   Register.Insert
     ("xorshift4096*", PRNG_Details'
        (Tag         => xorshift_star.xorshift4096_star'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xorshift4096* generator")));

   Register.Insert
     ("xoshiro256+", PRNG_Details'
        (Tag         => xoroshiro.xoroshiro128_plus'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xoshiro256+ generator")));

   Register.Insert
     ("xoshiro256**", PRNG_Details'
        (Tag         => xoroshiro.xoroshiro128_star_star'Tag,
         Params_Req  => False,
         Params      => No_Parameters'Access,
         Description => To_Unbounded_String ("xoshiro256** generator")));

end PRNG_Zoo.Register;
