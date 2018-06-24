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

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with PRNG_Zoo.xorshift;
with PRNG_Zoo.xorshift_star;
with PRNG_Zoo.xorshift_plus;
with PRNGTests_Suite.Sanity_Checks;
with PRNGTests_Suite.Sanity_Checks32;

package PRNGTests_Suite.xorshift_Family_Tests is

   type xorshift_Family_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out xorshift_Family_Test);

   function Name (T : xorshift_Family_Test) return Test_String;

   procedure Set_Up (T : in out xorshift_Family_Test);

   -- Test Routines:
   procedure Sanity_Check_SHR3 is
     new PRNGTests_Suite.Sanity_Checks32(P => xorshift.SHR3);
    procedure Sanity_Check_xor64 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift.xor64);
    procedure Sanity_Check_xorshift_3 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift.xorshift_3);
   procedure Sanity_Check_xss64 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_star.xorshift64_star);
   procedure Sanity_Check_xss1024 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_star.xorshift1024_star);
   procedure Sanity_Check_xss4096 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_star.xorshift4096_star);
   procedure Sanity_Check_xsp128 is
     new PRNGTests_Suite.Sanity_Checks(P => xorshift_plus.xorshift128_plus);

   procedure Sanity_Check_xorshift_array_32(T : in out Test_Case'Class);

end PRNGTests_Suite.xorshift_Family_Tests;
