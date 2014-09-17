--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests.Bits is

   type Bit_Counter(Width : Positive) is new PRNG_Test with private;
   procedure Reset(T : in out Bit_Counter);
   procedure Feed(T : in out Bit_Counter; X : in U64) with Inline;
   procedure Compute_Result(T : in out Bit_Counter);
   function Result_Ready(T: Bit_Counter) return Boolean
     with Inline;
   function Passed(T : in Bit_Counter; p : in Long_Float := 0.01) return Boolean;
   function p(T : in Bit_Counter) return Long_Float;
   function Describe_Result(T : in Bit_Counter) return String;

   type Bit_Counter_64 is new Bit_Counter(64) with private;

private

   type p_Array is array (Integer range <>) of Long_Float;

   type Bit_Counter(Width : Positive) is new PRNG_Test with
      record
         N : Counter := 0;
         B : Counter_array(1..Width);
         Ready : Boolean := False;
         Total_Bits : Counter;
         Total_Bits_p_value : Long_Float;
         Each_Bit_p_value : p_Array(1..Width); -- LSB is at index 1
      end record;

   type Bit_Counter_64 is new Bit_Counter(64) with null record;

end PRNG_Zoo.Tests.Bits;