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
   function Width(G: in Split_32) return Positive is (32);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return Split_32 is
     (if true then raise Program_Error else raise Program_Error);
   function Generate_Padded(G: in out Split_32) return U64 is
      (U64(U32'(Generate(G)))) with inline;
   function Generate(G: in out Split_32) return U32;

   -- This reverses the bit-order in the returned value
   -- Some tests are more sensitive to the lower than the upper bits or vice-versa
   type Bit_Reverse(IG : access PRNG'Class) is new Dispatcher(IG) with null record;
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return Bit_Reverse is
     (if true then raise Program_Error else raise Program_Error);
   function Generate(G: in out Bit_Reverse) return U64;
   function Generate_Padded(G: in out Bit_Reverse) return U64;
   function Generate(G: in out Bit_Reverse) return U32;

   -- Simple linear increment 'generator'.
   type Incrementer is new PRNG with
      record
         S : U64 := 0;
         incr : U64 := 1;
      end record;
   function Strength(G: in Incrementer) return PRNG_Strength is (Dummy);
   function Width(G: in Incrementer) return Positive is (64);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return Incrementer is
     (Incrementer'(incr => Params.Parameter("incr",1),
                   others => <>));
   procedure Reset(G: in out Incrementer; S: in U64);
   function Generate(G: in out Incrementer) return U64 with inline;
   function Generate_Padded(G : in out Incrementer) return U64 renames Generate;
   function Generate(G: in out Incrementer) return U32 with inline;

end PRNG_Zoo.Filters;
