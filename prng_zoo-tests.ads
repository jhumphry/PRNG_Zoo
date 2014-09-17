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

   type Test is limited interface;
   function Result_Ready(T: Test) return Boolean is abstract;
   function Passed(T : in Test; p : in Long_Float := 0.01) return Boolean is abstract
     with Pre'Class => Result_Ready(T);
   function p(T : in Test) return Long_Float is abstract
     with Pre'Class => Result_Ready(T);
   function Describe_Result(T : in Test) return String is abstract
     with Pre'Class => Result_Ready(T);

   type Test_Ptr is access all Test'Class;

   type PRNG_Test is limited interface and Test;
   procedure Reset(T : in out PRNG_Test) is abstract;
   procedure Feed(T : in out PRNG_Test; X : in U64) is abstract;
   procedure Compute_Result(T : in out PRNG_Test) is abstract;

   type Test_32 is limited interface and PRNG_Test;
   procedure Feed(T : in out Test_32; X : in U64) is null;
   procedure Feed(T : in out Test_32; X : in U32) is abstract;

   type PRNG_Test_Ptr is access all PRNG_Test'Class;

   Insufficient_Data : exception;

end PRNG_Zoo.Tests;
