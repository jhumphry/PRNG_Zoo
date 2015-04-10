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

package body PRNGTests_Suite.Stats is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Stats_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Z_Score'Access, "Check Z-score routines.");
      Register_Routine (T, Check_Chi2'Access, "Check Chi2 routines.");
      Register_Routine (T, Check_Erf'Access, "Check erf/erfi routines.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Stats_Test) return Test_String is
   begin
      return Format ("Tests of statistical routines");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Stats_Test) is
   begin
      null;
   end Set_Up;

   -------------------
   -- Check_Z_Score --
   -------------------

   procedure Check_Z_Score (T : in out Test_Cases.Test_Case'Class) is
      use PRNG_Zoo.Stats;
   begin
      -- SciPy and the NIST/Sematech Stats handbook were used to validate
      -- these checks.
      -- As the table only provides the survival function at intervals of 0.01
      -- the tests can't be expected to be finer than 0.005 at best
      Assert(Z_Score(Z => 1.955, alpha => 0.05, Two_Tailed => True),
             "Z => 1.955 should pass a two-tailed test at alpha => 0.05");
      Assert(not Z_Score(Z => 1.960, alpha => 0.05, Two_Tailed => True),
             "Z => 1.960 should not pass a two-tailed test at alpha => 0.05");
      Assert(Z_Score(Z => 1.640, alpha => 0.05, Two_Tailed => False),
             "Z => 1.640 should pass a one-tailed test at alpha => 0.05");
      Assert(not Z_Score(Z => 1.645, alpha => 0.05, Two_Tailed => False),
             "Z => 1.645 should not pass a one-tailed test at alpha => 0.05");

      Assert(Z_Score(Z => 3.290, alpha => 0.001, Two_Tailed => True),
             "Z => 3.290 should pass a two-tailed test at alpha => 0.001");
      Assert(not Z_Score(Z => 3.295, alpha => 0.001, Two_Tailed => True),
             "Z => 3.295 should not pass a two-tailed test at alpha => 0.001");
      Assert(Z_Score(Z => 3.090, alpha => 0.001, Two_Tailed => False),
             "Z => 3.090 should pass a one-tailed test at alpha => 0.001");
      Assert(not Z_Score(Z => 3.095, alpha => 0.001, Two_Tailed => False),
             "Z => 3.095 should not pass a one-tailed test at alpha => 0.001");

      Assert(Z_Score(Z => 0.0, alpha => 0.000001, Two_Tailed => True),
             "Z => 0.0 should always pass a two-tailed test at alpha => 0.000001");
      Assert(Z_Score(Z => 0.0, alpha => 0.000001, Two_Tailed => False),
             "Z => 0.0 should always pass a one-tailed test at alpha => 0.000001");
      Assert(not Z_Score(Z => 100.0, alpha => 0.1, Two_Tailed => True),
             "Z => 100.0 should always fail a two-tailed test at alpha => 0.1");
      Assert(not Z_Score(Z => 100.0, alpha => 0.1, Two_Tailed => False),
             "Z => 100.0 should always fail a one-tailed test at alpha => 0.1");

   end Check_Z_Score;

   ----------------
   -- Check_Chi2 --
   ----------------

   procedure Check_Chi2 (T : in out Test_Cases.Test_Case'Class) is
      use PRNG_Zoo.Stats;
   begin
      -- SciPy and the NIST/Sematech Stats handbook were used to validate
      -- these checks.
      Assert(Chi2_Test(0.00394, 1, 0.05),
             "Chi2 => 0.00394 should pass a 1df Chi2 at alpha => 0.05");
      Assert(not Chi2_Test(0.00393, 1, 0.05),
             "Chi2 => 0.00393 should not pass a 1df Chi2 at alpha => 0.05");
      Assert(Chi2_Test(3.841, 1, 0.05),
             "Chi2 => 3.841 should pass a 1df Chi2 at alpha => 0.05");
      Assert(not Chi2_Test(3.842, 1, 0.05),
             "Chi2 => 3.842 should not pass a 1df Chi2 at alpha => 0.05");

      Assert(Chi2_Test(61.920, 100, 0.001),
             "Chi2 => 61.920 should pass a 100df Chi2 at alpha => 0.001");
      Assert(not Chi2_Test(61.917, 100, 0.001),
             "Chi2 => 61.917 should not pass a 100df Chi2 at alpha => 0.001");
      Assert(Chi2_Test(149.449, 100, 0.001),
             "Chi2 => 149.449 should pass a 100df Chi2 at alpha => 0.001");
      Assert(not Chi2_Test(149.450, 100, 0.001),
             "Chi2 => 149.450 should not pass a 100df Chi2 at alpha => 0.001");

   end Check_Chi2;

   ---------------
   -- Check_Erf --
   ---------------

   procedure Check_Erf (T : in out Test_Cases.Test_Case'Class) is
      use PRNG_Zoo.Stats;
      X : Long_Float;
   begin

      -- The error function being accurate to 0.002 between -1.6 and +1.6
      -- is equivalent to the normal distribution CDF and inverse CDF being
      -- accurate to 0.001 in when applied together between -2.25 and +2.25.
      -- The inverse function given by Winitzki is only expected to be accurate
      -- to 0.002 so this is near the limit.

      for I in -8..8 loop
         X := Long_Float(I)/5.0;
         Assert(abs(erfi(erf(X))-X) <= 0.002,
                "abs(erfi(erf(X))-X) > 0.001 for X = " & Long_Float'Image(X) &
               " with value " & Long_Float'Image(abs(erfi(erf(X))-X)));
      end loop;

   end Check_Erf;


end PRNGTests_Suite.Stats;
