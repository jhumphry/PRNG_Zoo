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

with AUnit; use Aunit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package PRNGTests_Suite.Stats is

   type Stats_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Stats_Test);

   function Name (T : Stats_Test) return Test_String;

   procedure Set_Up (T : in out Stats_Test);

   procedure Check_Z_Score (T : in out Test_Cases.Test_Case'Class);
   procedure Check_Chi2 (T : in out Test_Cases.Test_Case'Class);
   procedure Check_Erf (T : in out Test_Cases.Test_Case'Class);

end PRNGTests_Suite.Stats;
