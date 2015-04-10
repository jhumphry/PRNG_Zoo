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

package body PRNG_Zoo.LFib is

   ------------------
   -- Generic_LFib --
   ------------------

   package body Generic_LFib is

      -----------
      -- Reset --
      -----------

      procedure Reset (G: in out LFib; S: in U64) is
         Dummy : U64 := 0;
      begin
         for I in G.s'Range loop
            G.s(I) := S xor U64(I);
         end loop;
         G.p := 0;
         for I in 1..k loop
            Dummy := Dummy + G.Generate;
         end loop;
      end Reset;

      --------------
      -- Generate --
      --------------

      function Generate (G: in out LFib) return U64 is
         Result : U64;
      begin
         Result := Op(G.s((G.p-j) mod k), G.s((G.p-k) mod k));
         G.s(G.p) := Result;
         G.p := (G.p + 1) mod k;
         return Result;
      end Generate;

   end Generic_LFib;

end PRNG_Zoo.LFib;
