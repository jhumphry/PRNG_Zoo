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

package body PRNGTests_Suite.xorshift_Family_Tests is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out xorshift_Family_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sanity_Check_SHR3'Access, "Basic sanity checks on SHR3 generator.");
      Register_Routine (T, Sanity_Check_xor64'Access, "Basic sanity checks on xor64 generator.");
      Register_Routine (T, Sanity_Check_xorshift_3'Access, "Basic sanity checks on xorshift_3 generator.");
      Register_Routine (T, Sanity_Check_xorshift_array_32'Access, "Basic sanity checks on xorshift_array_32 generator.");
      Register_Routine (T, Sanity_Check_xss64'Access, "Basic sanity checks on xorshift64* generator.");
      Register_Routine (T, Sanity_Check_xss1024'Access, "Basic sanity checks on xorshift1024* generator.");
      Register_Routine (T, Sanity_Check_xss4096'Access, "Basic sanity checks on xorshift4096* generator.");
      Register_Routine (T, Sanity_Check_xsp128'Access, "Basic sanity checks on xorshift128+ generator.");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : xorshift_Family_Test) return Test_String is
   begin
      return Format ("Tests on the xorshift[+|*] family of PRNG");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out xorshift_Family_Test) is
   begin
      null;
   end Set_Up;

   procedure Sanity_Check_xorshift_array_32(T : in out Test_Case'Class) is
      use all type PRNG_Zoo.xorshift.xorshift_array_32;
      G : PRNG_Zoo.xorshift.xorshift_array_32(4, 13, 17, 5);
      Output : U32_array(1..32);
      seed1 : constant U64 := 5489;
      seed2 : constant U64 := 655332;
   begin

      Reset(G, seed1);
      for I in Output'Range loop
         Output(I) := Generate(G);
      end loop;
      Assert((for all I in 2..Output'Last => Output(I) /= Output(1)),
             "Generator might be a constant or have very short period.");

      Reset(G, seed1);
      Assert((for all I in Output'Range => Output(I) = Generate(G)),
             "Resetting generator does not restart the same sequence.");

      Reset(G, seed2);
      Assert((for some I in Output'Range => Output(I) /= Generate(G)),
             "Different seeds appear to restart the same sequence.");

   end Sanity_Check_xorshift_array_32;

end PRNGTests_Suite.xorshift_Family_Tests;
