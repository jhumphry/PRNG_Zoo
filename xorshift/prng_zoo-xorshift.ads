--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.xorshift is

   subtype shift_32 is Integer range 1..31;
   subtype shift_64 is Integer range 1..63;
   type shift_direction is (Left, Right);

   type xorshift is interface and PRNG;

   -- Described as one of Marsaglia's favourites in the (Marsaglia, 2003)
   -- xorshift paper and used in several of his other papers as the C macro SHR3
   type SHR3 is new PRNG_32Only and xorshift with private;
   function Strength(G: in SHR3) return PRNG_Strength is (Low);
   procedure Reset(G: in out SHR3; S: in U64);
   function Generate(G: in out SHR3) return U32 with inline;

   -- Suggested for a 64 bit generator in the (Marsaglia, 2003) xorshift paper
   type xor64 is new PRNG_64Only and xorshift with private;
   function Strength(G: in xor64) return PRNG_Strength is (Low);
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
   procedure Reset(G: in out xorshift_3; S: in U64);
   function Generate(G: in out xorshift_3) return U64 with inline;

   type xorshift_array_32(N : Positive; a, b, c : Shift_32) is
     new PRNG_32Only and xorshift with private;
   function Strength(G: in xorshift_array_32) return PRNG_Strength is (Low);
   procedure Reset(G: in out xorshift_array_32; S: in U64);
   function Generate(G: in out xorshift_array_32) return U32 with inline;

private

   type SHR3 is new PRNG_32Only and xorshift with
      record
         s : U32 := 2463534242;
      end record;

   type xor64 is new PRNG_64Only and xorshift with
      record
         s : U64 := 88172645463325252;
      end record;

   type xorshift_array_32(N : Positive; a, b, c : Shift_32) is
     new PRNG_32Only and xorshift with
      record
         s : U32_array(1..N);
         p : Integer := 0;
      end record;

end PRNG_Zoo.xorshift;
