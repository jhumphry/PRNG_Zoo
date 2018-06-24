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

package PRNG_Zoo.LFib is

   generic
      j,k : Positive;
      with function Op(Left, Right: U64) return U64;
   package Generic_LFib is
      type LFib is new PRNG_64Only with private;
      function Strength(G: in LFib) return PRNG_Strength is (Low);
      function Constructor(Params : not null access PRNG_Parameters'Class) return LFib;
      procedure Reset(G: in out LFib; S: in U64);
      function Generate(G: in out LFib) return U64 with inline;

   private
      type LFib is new PRNG_64Only with
         record
            s : U64_array(0..k-1);
            p : Integer := 0;
         end record;

      function Constructor(Params : not null access PRNG_Parameters'Class)
                           return LFib is
         (LFib'(others => <>));

   end Generic_LFib;

end PRNG_Zoo.LFib;
