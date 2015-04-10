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

with PRNGTests_Suite.Lin_Con_Tests;
with PRNGTests_Suite.MT_Tests;
with PRNGTests_Suite.xorshift_Family_Tests;
with PRNGTests_Suite.LFib_Tests;
with PRNGTests_Suite.Misc_Tests;
with PRNGTests_Suite.Dispatcher_Tests;
with PRNGTests_Suite.Stats;

package body PRNGTests_Suite is
   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Test_Lin_Con : aliased Lin_Con_Tests.Lin_Con_Test;
   Test_MT : aliased MT_Tests.MT_Test;
   Test_xorshift : aliased xorshift_Family_Tests.xorshift_Family_Test;
   Test_LFib : aliased LFib_Tests.LFib_Test;
   Test_Misc : aliased Misc_Tests.Misc_Test;
   Test_Dispatcher : aliased Dispatcher_Tests.Dispatcher_Test;
   Test_Stats : aliased PRNGTests_Suite.Stats.Stats_Test;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Lin_Con'Access);
      Add_Test (Result'Access, Test_MT'Access);
      Add_Test (Result'Access, Test_xorshift'Access);
      Add_Test (Result'Access, Test_LFib'Access);
      Add_Test (Result'Access, Test_Misc'Access);
      Add_Test (Result'Access, Test_Dispatcher'Access);
      Add_Test (Result'Access, Test_Stats'Access);
      return Result'Access;
   end Suite;

end PRNGTests_Suite;
