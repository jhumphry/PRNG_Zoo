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

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.xoroshiro;
with PRNGTests_Suite.Sanity_Checks;

package PRNGTests_Suite.xoroshiro_Tests is

   type xoroshiro_Family_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out xoroshiro_Family_Test);

   function Name (T : xoroshiro_Family_Test) return Test_String;

   procedure Set_Up (T : in out xoroshiro_Family_Test);

   -- Test Routines:
   procedure Sanity_Check_xoroshiro128_plus is
     new PRNGTests_Suite.Sanity_Checks(P => xoroshiro.xoroshiro128_plus);

end PRNGTests_Suite.xoroshiro_Tests;
