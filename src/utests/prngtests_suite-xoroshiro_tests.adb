--
-- PRNG Zoo
-- Copyright (c) 2014 - 2018, James Humphry
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

package body PRNGTests_Suite.xoroshiro_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out xoroshiro_Family_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_xoroshiro128_plus'Access,
                        "Basic sanity checks on xoroshiro128+ generator.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : xoroshiro_Family_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests on the xoroshiro family of PRNG");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out xoroshiro_Family_Test) is
   begin
      null;
   end Set_Up;

end PRNGTests_Suite.xoroshiro_Tests;
