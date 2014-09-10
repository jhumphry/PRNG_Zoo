--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests is

   subtype Counter is Integer range 0..Integer'Last;
   type Counter_array is array (Integer range <>) of Counter
     with Default_Component_Value => 0;
   type Counter_array_ptr is access Counter_array;
   type LF_array is array (Integer range <>) of Long_Float;

   type Binned(N : Positive) is tagged
      record
         Bin_Counts : Counter_array(1..N);
         Bin_Expected : LF_array(1..N);
         Distribution_DF : Positive;
         Bin_Boundary : LF_array(1..N);
      end record;

   type Test_Result is interface;
   function Passed(TR : in Test_Result; p : in Long_Float := 0.01)
                   return Boolean is abstract;
   function p(TR : in Test_Result) return Long_Float is abstract;
   function Describe(TR : in Test_Result) return String is abstract;

   type Test_Result_Ptr is access all Test_Result'Class;

   type Test is limited interface;
   procedure Reset(T : in out Test) is abstract;
   procedure Feed(T : in out Test; X : in U64) is abstract;
   function Result(T : in Test) return Test_Result_Ptr is abstract;

   type Test_32 is limited interface and Test;
   procedure Feed(T : in out Test_32; X : in U64) is null;
   procedure Feed(T : in out Test_32; X : in U32) is abstract;

   type Test_Ptr is access all Test'Class;

end PRNG_Zoo.Tests;
