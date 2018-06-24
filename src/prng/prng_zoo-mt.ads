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

package PRNG_Zoo.MT is

   -- Based on (Matsumoto and Nishimura, 1998)
   type MT19937 is new PRNG_32Only and PRNG_Seed_From_Array with private;
   function Strength(G: in MT19937) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return MT19937;
   procedure Reset(G: in out MT19937; S: in U64);
   procedure Reset(G: in out MT19937; S: in U64_array);
   function Generate(G: in out MT19937) return U32 with inline;

   type MT19937_64 is new PRNG_64Only and PRNG_Seed_From_Array with private;
   function Strength(G: in MT19937_64) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return MT19937_64;
   procedure Reset(G: in out MT19937_64; S: in U64);
   procedure Reset(G: in out MT19937_64; S: in U64_array);
   function Generate(G: in out MT19937_64) return U64 with inline;

   -- Based on (Matsumoto and Nishimura, 2011)
   type TinyMT_64 is new PRNG_64Only and PRNG_Seed_From_Array with private;
   function Strength(G: in TinyMT_64) return PRNG_Strength is (Medium);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return TinyMT_64;
   procedure Reset(G: in out TinyMT_64; S: in U64);
   procedure Reset(G: in out TinyMT_64; S: in U64_array);
   function Generate(G: in out TinyMT_64) return U64 with inline;

private

   type MT_Index is mod 624;
   type MT_State is array (MT_Index) of U32;
   type MT19937 is new PRNG_32Only and PRNG_Seed_From_Array with
      record
         s : MT_State;
         p : MT_Index;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                              return MT19937 is
     (MT19937'(others => <>));

   type MT_Index_64 is mod 312;
   type MT_State_64 is array (MT_Index_64) of U64;
   type MT19937_64 is new PRNG_64Only and PRNG_Seed_From_Array with
      record
         s : MT_State_64;
         p : MT_Index_64;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                              return MT19937_64 is
     (MT19937_64'(others => <>));

   type TinyMT_64_State is array (U64 range 0..1) of U64;
   type TinyMT_64 is new PRNG_64Only and PRNG_Seed_From_Array with
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
