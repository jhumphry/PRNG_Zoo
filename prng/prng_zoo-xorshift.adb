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

package body PRNG_Zoo.xorshift is

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out SHR3; S: in U64) is
   begin
      G.s := U32(S and 16#FFFFFFFF#);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out SHR3) return U32 is
   begin
      G.s := G.s xor Shift_Left(G.s, 13);
      G.s := G.s xor Shift_Right(G.s, 17);
      G.s := G.s xor Shift_Left(G.s, 5);
      return G.s;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out xor64; S: in U64) is
   begin
      G.s := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xor64) return U64 is
   begin
      G.s := G.s xor Shift_Left(G.s, 13);
      G.s := G.s xor Shift_Right(G.s, 7);
      G.s := G.s xor Shift_Left(G.s, 17);
      return G.s;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out xorshift_3; S: in U64) is
   begin
      G.s := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xorshift_3) return U64 is
   begin
      if G.p = Left then
         G.s := G.s xor Shift_Left(G.s, G.a);
      else
         G.s := G.s xor Shift_Right(G.s, G.a);
      end if;

      if G.q = Left then
         G.s := G.s xor Shift_Left(G.s, G.b);
      else
         G.s := G.s xor Shift_Right(G.s, G.b);
      end if;

      if G.r = Left then
         G.s := G.s xor Shift_Left(G.s, G.c);
      else
         G.s := G.s xor Shift_Right(G.s, G.c);
      end if;

      return G.s;
   end Generate;

   -----------------
   -- Constructor --
   -----------------

   function Constructor(Params : not null access PRNG_Parameters'Class) return xorshift_array_32 is
      P : xorshift_array_32_Parameters;
   begin
      if Params.all in xorshift_array_32_Parameters then
         P := xorshift_array_32_Parameters(Params.all);
         return xorshift_array_32'(N => P.N,
                                   a => P.a,
                                   b => P.b,
                                   c => P.c,
                                   others => <>);
      else
         raise Invalid_Parameters;
      end if;
   end Constructor;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out xorshift_array_32; S: in U64) is
      G2 : SHR3;
   begin
      Reset(G2, S);
      for I in G.s'Range loop
         G.s(I) := Generate(G2);
      end loop;
      G.p := 0;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xorshift_array_32) return U32 is
      t : U32;
      last : Integer;
   begin
      t := G.s(G.p + 1);
      t := t xor Shift_Left(t, G.a);
      last := (G.p - 1) mod G.N + 1;
      G.s(last) := (G.s(last) xor Shift_Right(G.s(last), G.c)) xor (t xor Shift_Right(t, G.b));
      G.p := (G.p + 1) mod G.N;
      return G.s(last);
   end Generate;

end PRNG_Zoo.xorshift;
