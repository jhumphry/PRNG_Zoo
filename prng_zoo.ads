--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with Interfaces;
use all type Interfaces.Unsigned_64;
use all type Interfaces.Unsigned_32;

package PRNG_Zoo is

   subtype U64 is Interfaces.Unsigned_64;
   type U64_array is array (Integer range <>) of U64;
   subtype U64_Nonzero is
     Interfaces.Unsigned_64 range 1..Interfaces.Unsigned_64'Last - 1;
   subtype U32 is Interfaces.Unsigned_32;
   type U32_array is array (Integer range <>) of U32;
   subtype U32_Nonzero is
     Interfaces.Unsigned_32 range 1..Interfaces.Unsigned_32'Last - 1;

   type PRNG_Strength is (Crypto, High, Medium, Low, Dummy);

   type PRNG is interface;
   function Strength(G: in PRNG) return PRNG_Strength is abstract;
   procedure Reset(G: in out PRNG; S: in U64) is abstract;
   function Generate(G: in out PRNG) return U64 is abstract;

   type PRNG_32 is interface and PRNG;
   function Generate(G: in out PRNG_32) return U32 is abstract;

   type PRNG_32Only is abstract new PRNG_32 with null record;
   function Generate(G: in out PRNG_32Only) return U64;

   type PRNG_Ptr is access all PRNG'Class;
   type PRNG_32_Ptr is access all PRNG_32'Class;

   -- The Dispatcher types exist so that other routines can be written as
   -- generics with compile-type polymorphism (for optimum performance in actual
   -- simulations) while still allowing for run-time polymorphism for tests of
   -- statistical quality and relative speed.

   type Dispatcher(IG : access PRNG'Class) is new PRNG with null record;
   function Strength(G: in Dispatcher) return PRNG_Strength is
     (Strength(G.IG.all));
   procedure Reset(G: in out Dispatcher; S: in U64);
   function Generate(G: in out Dispatcher) return U64 is
     (Generate(G.IG.all));

   type Dispatcher_32(IG : access PRNG_32'Class) is new PRNG_32 with null record;
   function Strength(G: in Dispatcher_32) return PRNG_Strength is
     (Strength(G.IG.all));
   procedure Reset(G: in out Dispatcher_32; S: in U64);
   function Generate(G: in out Dispatcher_32) return U64 is
     (Generate(G.IG.all));
   function Generate(G: in out Dispatcher_32) return U32 is
     (Generate(G.IG.all));

   scale_unsigned_32 : constant := 2.32830_64365_38696_28906_25000E-10;
   scale_unsigned_64 : constant := 5.42101_08624_27522_17003_72640_04349_70855_71289_06250E-20;

end PRNG_Zoo;
