--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Interfaces;
use all type Interfaces.Unsigned_64;
use all type Interfaces.Unsigned_32;

package PRNG_Zoo is

   subtype U64 is Interfaces.Unsigned_64;
   subtype U32 is Interfaces.Unsigned_32;

   type PRNG_Strength is (Crypto, High, Medium, Low, Dummy);

   type PRNG is interface;
   function Strength(G: in PRNG) return PRNG_Strength is abstract;
   procedure Reset(G: in out PRNG; S: in U64) is abstract;
   function Generate(G: in out PRNG) return U64 is abstract;

   type PRNG_32 is interface and PRNG;
   function Generate(G: in out PRNG_32) return U32 is abstract;

   type PRNG_32Only is abstract new PRNG_32 with null record;
   function Generate(G: in out PRNG_32Only) return U64;

   scale_unsigned_32 : constant := 2.32830_64365_38696_28906_25000E-10;
   scale_unsigned_64 : constant := 5.42101_08624_27522_17003_72640_04349_70855_71289_06250E-20;

end PRNG_Zoo;
