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
      Register_Routine (T, Sanity_Check'Access, "Basic sanity checks on MT19937 generator.");
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
      -- canonical C version of MT19937 (2002 version) with initial seed array
      -- {0x123, 0x234, 0x345, 0x456}, as used in the demonstration code
      -- from the authors

      Expected_Array : constant U32_Array := (
                                              1067595299,  955945823,  477289528, 4107218783,
                                              4228976476, 3344332714, 3355579695,  227628506,
                                              810200273, 2591290167, 2560260675, 3242736208,
                                              646746669, 1479517882, 4245472273, 1143372638,
                                              3863670494, 3221021970, 1773610557, 1138697238,
                                              1421897700, 1269916527, 2859934041, 1764463362,
                                              3874892047, 3965319921,   72549643, 2383988930,
                                              2600218693, 3237492380, 2792901476,  725331109,
                                              605841842,  271258942,  715137098, 3297999536,
                                              1322965544, 4229579109, 1395091102, 3735697720,
                                              2101727825, 3730287744, 2950434330, 1661921839,
                                              2895579582, 2370511479, 1004092106, 2247096681,
                                              2111242379, 3237345263, 4082424759,  219785033,
                                              2454039889, 3709582971,  835606218, 2411949883,
                                              2735205030,  756421180, 2175209704, 1873865952,
                                              2762534237, 4161807854, 3351099340,  181129879
                                             );


      -- This array was generated from a quick C program using the
      -- canonical C version of MT19937 (2002 version) with initial seed
      -- 5489.

      Expected_5489 : constant U32_Array := (
                                             3499211612,  581869302, 3890346734, 3586334585,
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

      Reset(G, U64_array'(16#0123#, 16#0234#, 16#0345#, 16#0456#));
      for E of Expected_Array loop
         Assert(U32'(Generate(G)) = E,
                "MT19937 implementation produces unexpected result for seed {0x123, 0x234, 0x345, 0x456}");
      end loop;

      Reset(G, 5489);
      for E of Expected_5489 loop
         Assert(U32'(Generate(G)) = E,
                "MT19937 implementation produces unexpected result for seed 5489");
      end loop;
   end Test_MT19937;

end MT_Tests;
