--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.MT is

   type MT19937 is new PRNG_32Only with private;
   function Strength(G: in MT19937) return PRNG_Strength is (Medium);
   procedure Reset(G: in out MT19937; S: in U64);
   procedure Reset(G: in out MT19937; S: in U64_array);
   function Generate(G: in out MT19937) return U32;

private

   type MT_Index is mod 624;
   type MT_State is array (MT_Index) of U32;
   type MT19937 is new PRNG_32Only with
      record
         s : MT_State;
         p : MT_Index;
      end record;

end PRNG_Zoo.MT;
