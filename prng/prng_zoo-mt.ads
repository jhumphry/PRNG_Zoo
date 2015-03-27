--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.MT is

   -- Based on (Matsumoto and Nishimura, 1998)
   type MT19937 is new PRNG_32Only with private;
   function Strength(G: in MT19937) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return MT19937;
   procedure Reset(G: in out MT19937; S: in U64);
   procedure Reset(G: in out MT19937; S: in U64_array);
   function Generate(G: in out MT19937) return U32 with inline;

   type MT19937_64 is new PRNG_64Only with private;
   function Strength(G: in MT19937_64) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return MT19937_64;
   procedure Reset(G: in out MT19937_64; S: in U64);
   procedure Reset(G: in out MT19937_64; S: in U64_array);
   function Generate(G: in out MT19937_64) return U64 with inline;

   -- Based on (Matsumoto and Nishimura, 2011)
   type TinyMT_64 is new PRNG_64Only with private;
   function Strength(G: in TinyMT_64) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return TinyMT_64;
   procedure Reset(G: in out TinyMT_64; S: in U64);
   --procedure Reset(G: in out TinyMT_64; S: in U64_array); -- TODO
   function Generate(G: in out TinyMT_64) return U64 with inline;

private

   type MT_Index is mod 624;
   type MT_State is array (MT_Index) of U32;
   type MT19937 is new PRNG_32Only with
      record
         s : MT_State;
         p : MT_Index;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                              return MT19937 is
     (MT19937'(others => <>));

   type MT_Index_64 is mod 312;
   type MT_State_64 is array (MT_Index_64) of U64;
   type MT19937_64 is new PRNG_64Only with
      record
         s : MT_State_64;
         p : MT_Index_64;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                              return MT19937_64 is
     (MT19937_64'(others => <>));

   type TinyMT_64_State is array (U64 range 0..1) of U64;
   type TinyMT_64 is new PRNG_64Only with
      record
         status : TinyMT_64_State;
         mat1 : U32 := 16#fa051f40#;
         mat2 : U32 := 16#ffd0fff4#;
         tmat : U64 := 16#58d02ffeffbfffbc#;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                              return TinyMT_64 is
     (TinyMT_64'(others => <>));


end PRNG_Zoo.MT;
