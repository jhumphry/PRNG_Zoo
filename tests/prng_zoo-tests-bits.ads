--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests.Bits is

   subtype Counter is Integer range 0..Integer'Last;
   type Counter_array is array (Integer range <>) of Counter
     with Default_Component_Value => 0;

   type Bit_Counter is new Test with private;
   procedure Reset(T: in out Bit_Counter);
   procedure Feed(T: in out Bit_Counter; X : in U64) with Inline;
   function Result(T: in Bit_Counter; p : in Long_Float := 0.01)
                   return Test_Result_Ptr;

   type Bit_Counter_Result is new Test_Result with private;
   function Passed(TR : in Bit_Counter_Result) return Boolean;
   function Describe(TR : in Bit_Counter_Result) return String;

private
   type Bit_Counter is new Test with
      record
         N : Counter := 0;
         B : Counter_array(1..64) := (others => 0);
      end record;

   type Pass_Array is array (Integer range 1..64) of Boolean;

   type Bit_Counter_Result is new Test_Result with
      record
         p : Long_Float;
         N : Counter;
         All_Pass : Boolean;
         Total_Bits : Counter;
         Total_Bits_Pass : Boolean;
         Each_Bit_Pass : Pass_Array := (others => False); -- LSB is at index 1
      end record;

end PRNG_Zoo.Tests.Bits;
