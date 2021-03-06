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

package PRNG_Zoo.xorshift is

   subtype shift_32 is Integer range 1..31;
   subtype shift_64 is Integer range 1..63;
   type shift_direction is (Left, Right);

   type xorshift is interface and PRNG;

   -- Described as one of Marsaglia's favourites in the (Marsaglia, 2003)
   -- xorshift paper and used in several of his other papers as the C macro SHR3
   type SHR3 is new PRNG_32Only and xorshift with private;
   function Strength(G: in SHR3) return PRNG_Strength is (Low);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return SHR3;
   procedure Reset(G: in out SHR3; S: in U64);
   function Generate(G: in out SHR3) return U32 with inline;

   -- Suggested for a 64 bit generator in the (Marsaglia, 2003) xorshift paper
   type xor64 is new PRNG_64Only and xorshift with private;
   function Strength(G: in xor64) return PRNG_Strength is (Low);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xor64;
   procedure Reset(G: in out xor64; S: in U64);
   function Generate(G: in out xor64) return U64 with inline;

   type xorshift_3 is new PRNG_64Only and xorshift with
      record
         s : U64 := 88172645463325252;
         a : shift_64 := 13;
         b : shift_64 := 7;
         c : shift_64 := 17;
         p, r : shift_direction := Left;
         q : shift_direction := Right;
      end record;
   function Strength(G: in xorshift_3) return PRNG_Strength is (Low);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift_3;
   procedure Reset(G: in out xorshift_3; S: in U64);
   function Generate(G: in out xorshift_3) return U64 with inline;

   type xorshift_array_32_Parameters is new PRNG_Parameters with
      record
         N : Positive;
         a, b, c : shift_32;
      end record;

   type xorshift_array_32(N : Positive; a, b, c : shift_32) is
     new PRNG_32Only and xorshift with private;
   function Strength(G: in xorshift_array_32) return PRNG_Strength is (Low);
   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xorshift_array_32;
   procedure Reset(G: in out xorshift_array_32; S: in U64);
   function Generate(G: in out xorshift_array_32) return U32 with inline;

private

   type SHR3 is new PRNG_32Only and xorshift with
      record
         s : U32 := 2463534242;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return SHR3 is
     (SHR3'(others => <>));

   type xor64 is new PRNG_64Only and xorshift with
      record
         s : U64 := 88172645463325252;
      end record;

   function Constructor(Params : not null access PRNG_Parameters'Class)
                           return xor64 is
     (xor64'(others => <>));

   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return xorshift_3 is
     (xorshift_3'(others => <>));

   type xorshift_array_32(N : Positive; a, b, c : shift_32) is
     new PRNG_32Only and xorshift with
      record
         s : U32_array(1..N);
         p : Integer := 0;
      end record;

end PRNG_Zoo.xorshift;
