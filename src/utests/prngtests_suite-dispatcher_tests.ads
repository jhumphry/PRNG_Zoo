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

package PRNGTests_Suite.Dispatcher_Tests is

   type Dispatcher_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Dispatcher_Test);

   function Name (T : Dispatcher_Test) return Test_String;

   procedure Set_Up (T : in out Dispatcher_Test);

   -- Test Routines:
   procedure Test_Dispatcher (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Dispatcher_32 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Split_32 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Bit_Reverse (T : in out Test_Cases.Test_Case'Class);

end PRNGTests_Suite.Dispatcher_Tests;
