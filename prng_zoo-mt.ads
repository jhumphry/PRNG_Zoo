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

   type MT19937_64 is new PRNG with private;
   function Strength(G: in MT19937_64) return PRNG_Strength is (Medium);
   procedure Reset(G: in out MT19937_64; S: in U64);
   procedure Reset(G: in out MT19937_64; S: in U64_array);
   function Generate(G: in out MT19937_64) return U64;

private

   type MT_Index is mod 624;
   type MT_State is array (MT_Index) of U32;
   type MT19937 is new PRNG_32Only with
      record
         s : MT_State;
         p : MT_Index;
      end record;

   type MT_Index_64 is mod 312;
   type MT_State_64 is array (MT_Index_64) of U64;
   type MT19937_64 is new PRNG with
      record
         s : MT_State_64;
         p : MT_Index_64;
      end record;

end PRNG_Zoo.MT;
