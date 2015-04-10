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
with AUnit.Test_Cases; use AUnit.Test_Cases;

procedure PRNGTests_Suite.Sanity_Checks32(T : in out Test_Case'Class) is
   G : P;
   Output : U32_array(1..N);
   Mask : U64 := Shift_Left(16#FFFFFFFF#, G.Width);
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

   Reset(G, seed1);
   Assert((for all I in 1..100 => ((Generate_Padded(G) + U64(I)) and Mask) = 0),
          "Declared generator width not consistent with output.");

end PRNGTests_Suite.Sanity_Checks32;
