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

with AUnit.Assertions; use AUnit.Assertions;

with PRNG_Zoo.Misc;
use all type PRNG_Zoo.Misc.glibc_random;
use all type PRNG_Zoo.Misc.KISS;

package body PRNGTests_Suite.Misc_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Misc_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_GLib_Random'Access, "Basic sanity checks on GLib Random() generator.");
      Register_Routine (T, Sanity_Check_KISS'Access, "Basic sanity checks on Marsaglia KISS generator.");
      Register_Routine (T, Test_Glib_Random'Access, "Test GLib Random() generator against expected (initial) output");
      Register_Routine (T, Sanity_Check_MurmurHash3'Access, "Basic sanity checks on generator based on MurmurHash3.");
      Register_Routine (T, Sanity_Check_SplitMix'Access, "Basic sanity checks on SplitMix generator.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Misc_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Test miscellaneous PRNGs");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Misc_Test) is
   begin
      null;
   end Set_Up;

   ----------------------
   -- Test_Glib_Random --
   ----------------------

   procedure Test_Glib_Random (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      G : Misc.glibc_random;

      Expected_Array : constant U32_array := (
                                              1804289383,  846930886, 1681692777, 1714636915,
                                              1957747793,  424238335,  719885386, 1649760492,
                                              596516649, 1189641421, 1025202362, 1350490027,
                                              783368690, 1102520059, 2044897763, 1967513926,
                                              1365180540, 1540383426,  304089172, 1303455736,
                                              35005211,  521595368,  294702567, 1726956429,
                                              336465782,  861021530,  278722862,  233665123,
                                              2145174067,  468703135, 1101513929, 1801979802,
                                              1315634022,  635723058, 1369133069, 1125898167,
                                              1059961393, 2089018456,  628175011, 1656478042,
                                              1131176229, 1653377373,  859484421, 1914544919,
                                              608413784,  756898537, 1734575198, 1973594324,
                                              149798315, 2038664370, 1129566413,  184803526,
                                              412776091, 1424268980, 1911759956,  749241873,
                                              137806862,   42999170,  982906996,  135497281,
                                              511702305, 2084420925, 1937477084, 1827336327
                                             );

   begin
      Reset(G, 1);
      for E of Expected_Array loop
         Assert(U32'(Generate(G)) = E,
                "GLib Random() implementation produces unexpected result for seed 1");
      end loop;

   end Test_Glib_Random;

end PRNGTests_Suite.Misc_Tests;
