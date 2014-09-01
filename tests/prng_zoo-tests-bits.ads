--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests.Bits is

   subtype Counter is Integer range 0..Integer'Last;
   type Counter_array is array (Integer range <>) of Counter
     with Default_Component_Value => 0;

   type Bit_Counter is new Test with private;
   procedure Reset(T : in out Bit_Counter);
   procedure Feed(T : in out Bit_Counter; X : in U64) with Inline;
   function Result(T : in Bit_Counter; Width : in Positive) return Test_Result_Ptr;
   function Result(T : in Bit_Counter) return Test_Result_Ptr is (Result(T, 64));

   type Bit_Counter_Result is new Test_Result with private;
   function Passed(TR : in Bit_Counter_Result; p : in Long_Float := 0.01) return Boolean;
   function p(TR : in Bit_Counter_Result) return Long_Float;
   function Describe(TR : in Bit_Counter_Result) return String;

private
   type Bit_Counter is new Test with
      record
         N : Counter := 0;
         B : Counter_array(1..64) := (others => 0);
      end record;

   type p_Array is array (Integer range <>) of Long_Float;

   type Bit_Counter_Result is new Test_Result with
      record
         Width : Positive := 64;
         N : Counter;
         Total_Bits : Counter;
         Total_Bits_p_value : Long_Float;
         Each_Bit_p_value : p_Array(1..64) := (others => 0.0); -- LSB is at index 1
      end record;

end PRNG_Zoo.Tests.Bits;
