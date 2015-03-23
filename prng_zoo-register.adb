--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with PRNG_Zoo.LFib;
with PRNG_Zoo.Linear_Congruential, PRNG_Zoo.Linear_Congruential.Examples;
with PRNG_Zoo.Misc;
with PRNG_Zoo.MT;
with PRNG_Zoo.xorshift;
with PRNG_Zoo.xorshift_plus;
with PRNG_Zoo.xorshift_star;

package body PRNG_Zoo.Register is

   package LFIB_107_378 is new LFIB.Generic_LFib(107,378,"+");

   procedure PRNG_Column_Widths(Names, Descriptions : in out Natural) is
      I : Register_Cursor;
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


   procedure Display_Register is
      I : Register_Cursor;
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
   ("LFib_107_378", PRNG_Details'
      (Tag         => LFIB_107_378.LFib'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("Additive Lagged Fibbonacci S_n = S_{n-107} + S_{n-378}")));

   Register.Insert
   ("MINSTD", PRNG_Details'
      (Tag         => Linear_Congruential.LCG_32Only'Tag,
       Params      => Linear_Congruential.Examples.MINSTD0_Parameters'Access,
       Description => To_Bounded_String ("MINSTD Linear Congruential generator (as per C++ spec)")));

   Register.Insert
   ("MINSTD0", PRNG_Details'
      (Tag         => Linear_Congruential.LCG_32Only'Tag,
       Params      => Linear_Congruential.Examples.MINSTD0_Parameters'Access,
       Description => To_Bounded_String ("MINSTD0, a slightly older varient of MINSTD")));

   Register.Insert
   ("RANDU", PRNG_Details'
      (Tag         => Linear_Congruential.LCG_32Only'Tag,
       Params      => Linear_Congruential.Examples.RANDU_Parameters'Access,
       Description => To_Bounded_String ("RANDU, a very poor but traditional LCG PRNG")));

   Register.Insert
   ("glibc", PRNG_Details'
      (Tag         => Misc.glibc_random'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("GLibc random() generator")));

   Register.Insert
   ("KISS", PRNG_Details'
      (Tag         => Misc.KISS'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("Marsaglia's KISS generator")));

   Register.Insert
   ("Murmurhash3", PRNG_Details'
      (Tag         => Misc.MurmurHash3'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("Generator based on Murmurhash3")));

   Register.Insert
   ("mt", PRNG_Details'
      (Tag         => MT.MT19937'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("Mersenne Twister (mt19937)")));

   Register.Insert
   ("mt64", PRNG_Details'
      (Tag         => MT.MT19937_64'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("64-bit varient of the Mersenne Twister (mt19937_64)")));

   Register.Insert
   ("SHR3", PRNG_Details'
      (Tag         => xorshift.SHR3'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("Marsaglia's SHR3 xorshift generator")));

   Register.Insert
   ("xor64", PRNG_Details'
      (Tag         => xorshift.SHR3'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("Marsaglia's xor64 xorshift generator")));

   Register.Insert
   ("xorshift128+", PRNG_Details'
      (Tag         => xorshift_plus.xorshift128_plus'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("xorshift128+ generator")));

   Register.Insert
   ("xorshift64*", PRNG_Details'
      (Tag         => xorshift_star.xorshift64_star'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("xorshift64* generator")));

   Register.Insert
   ("xorshift1024*", PRNG_Details'
      (Tag         => xorshift_star.xorshift1024_star'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("xorshift1024* generator")));

   Register.Insert
   ("xorshift4096*", PRNG_Details'
      (Tag         => xorshift_star.xorshift4096_star'Tag,
       Params      => No_Parameters'Access,
       Description => To_Bounded_String ("xorshift4096* generator")));


end PRNG_Zoo.Register;
