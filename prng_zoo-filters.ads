--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

-- This package contains various simple dummy generators
-- and filters useful for tests

package PRNG_Zoo.Filters is

   -- This generates 32 bit values from a 64 bit generator by returning the
   -- lower and upper words in sequence rather than just throwing away the upper
   -- word
   type Split_32(IG : access PRNG'Class) is new Dispatcher(IG) with
      record
         Loaded : Boolean := False;
         Next_Value : U32;
      end record;
   function Generate(G: in out Split_32) return U32;

   -- This reverses the bit-order in the returned value
   -- Some tests are more sensitive to the lower than the upper bits or vice-versa
   type Bit_Reverse(IG : access PRNG'Class) is new Dispatcher(IG) with null record;
   function Generate(G: in out Bit_Reverse) return U64;
   function Generate(G: in out Bit_Reverse) return U32;

   -- Simple linear increment 'generator'.
   type Incrementer is new PRNG with
      record
         S : U64 := 0;
         incr : U64 := 1;
      end record;
   function Strength(G: in Incrementer) return PRNG_Strength is (Dummy);
   procedure Reset(G: in out Incrementer; S: in U64);
   function Generate(G: in out Incrementer) return U64 with inline;
   function Generate(G: in out Incrementer) return U32 with inline;

end PRNG_Zoo.Filters;
