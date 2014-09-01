--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package PRNG_Zoo.Tests is

   type Test_Result is interface;
   function Passed(TR : in Test_Result; p : in Long_Float := 0.01)
                   return Boolean is abstract;
   function p(TR : in Test_Result) return Long_Float is abstract;
   function Describe(TR : in Test_Result) return String is abstract;

   type Test_Result_Ptr is access all Test_Result'Class;

   type Test is interface;
   procedure Reset(T : in out Test) is abstract;
   procedure Feed(T : in out Test; X : in U64) is abstract;
   function Result(T : in Test) return Test_Result_Ptr is abstract;

   type Test_32 is interface and Test;
   procedure Feed(T : in out Test_32; X : in U64) is null;
   procedure Feed(T : in out Test_32; X : in U32) is abstract;

   type Test_Ptr is access all Test;

end PRNG_Zoo.Tests;
