--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

with AUnit.Assertions; use AUnit.Assertions;

with PRNG_Zoo;
use PRNG_Zoo;
use all type PRNG_Zoo.U32;

with PRNG_Zoo.MT;
use all type PRNG_Zoo.MT.MT19937;

package body MT_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out MT_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_MT19937'Access, "Test MT19937 generator against expected (initial) output");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : MT_Test) return Test_String is
   begin
      return Format ("Mersenne Twister PRNG Tests");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out MT_Test) is
   begin
      null;
   end Set_Up;

   ------------------
   -- Test_MT19937 --
   ------------------

   procedure Test_MT19937 (T : in out Test_Cases.Test_Case'Class) is
      G : MT.MT19937;

      -- This array was generated from a quick C program using the
      -- canonical C version of MT19937 (2002 version) with initial seed
      -- 5489.

      Expected : U32_Array := (3499211612,  581869302, 3890346734, 3586334585,
                               545404204, 4161255391, 3922919429,  949333985,
                               2715962298, 1323567403,  418932835, 2350294565,
                               1196140740,  809094426, 2348838239, 4264392720,
                               4112460519, 4279768804, 4144164697, 4156218106,
                               676943009, 3117454609, 4168664243, 4213834039,
                               4111000746,  471852626, 2084672536, 3427838553,
                               3437178460, 1275731771,  609397212,   20544909,
                               1811450929,  483031418, 3933054126, 2747762695,
                               3402504553, 3772830893, 4120988587, 2163214728,
                               2816384844, 3427077306,  153380495, 1551745920,
                               3646982597,  910208076, 4011470445, 2926416934,
                               2915145307, 1712568902, 3254469058, 3181055693,
                               3191729660, 2039073006, 1684602222, 1812852786,
                               2815256116,  746745227,  735241234, 1296707006,
                               3032444839, 3424291161,  136721026, 1359573808
                              );
   begin
      Reset(G, 5489);
      for E of Expected loop
         Assert(U32'(Generate(G)) = E, "MT19937 implementation produces unexpected result");
      end loop;
   end Test_MT19937;

end MT_Tests;
