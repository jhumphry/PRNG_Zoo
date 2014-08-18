--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

-- Includes material derived from mt19937ar.c
-- The copyright notice of which follows:

--     Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
--     All rights reserved.
--
--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions
--     are met:
--
--       1. Redistributions of source code must retain the above copyright
--          notice, this list of conditions and the following disclaimer.
--
--       2. Redistributions in binary form must reproduce the above copyright
--          notice, this list of conditions and the following disclaimer in the
--          documentation and/or other materials provided with the distribution.
--
--       3. The names of its contributors may not be used to endorse or promote
--          products derived from this software without specific prior written
--          permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--     A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
--     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
--     EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
--     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--     PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--     SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


package body PRNG_Zoo.MT is

   N : constant := 624;
   M : constant := 397;
   MATRIX_A : constant := 16#9908B0DF#;
   UPPER_MASK : constant := 16#80000000#;
   LOWER_MASK : constant := 16#7FFFFFFF#;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out MT19937; S: in U64) is
   begin
      G.s(0) := U32(S and 16#FFFFFFFF#);
      G.p := 0;
      For I in MT_Index range 1..MT_Index(N-1) loop
         G.s(I) := 1812433253 * (G.s(I-1) xor Shift_Right(G.s(I-1), 30))
           + U32(I);
      end loop;
   end Reset;

   procedure Reset (G: in out MT19937; S: in U64_array) is
      key : U64_array(0..S'Length-1) := S;
      i : MT_Index;
      j : Integer;
   begin
      Reset(G, U64(19650218));

      i := 1;
      j := 0;

      for k in reverse 1..(if N > key'Length then N else key'Length) loop
         G.s(i) := (G.s(i) xor ((G.s(i-1) xor Shift_Right(G.s(i-1), 30)) * 1664525)) +
           U32(key(j) and 16#FFFFFFFF#) + U32(j);
         i := i + 1;
         j := (j + 1) mod key'Length;
         if i=0 then
            G.s(0) := G.s(MT_Index(N-1));
            i := 1;
         end if;
      end loop;

      for k in reverse 1..MT_Index(N-1) loop
         G.s(i) := (G.s(i) xor ((G.s(i-1) xor Shift_Right(G.s(i-1), 30)) * 1566083941))
           - U32(i);
         i := i + 1;
         if i=0 then
            G.s(0) := G.s(MT_Index(N-1));
            i := 1;
         end if;
      end loop;

      G.s(0) := 16#80000000#;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out MT19937) return U32 is
         y : U32;
   begin
      if G.p = 0 then
         for I in MT_Index'Range loop
            y := (G.s(I) and UPPER_MASK) or (G.s(I+1) and LOWER_MASK);
            G.s(I) := G.s(I+M) xor Shift_Right(y, 1);
            if (y mod 2) = 1 then
               G.s(I) := G.s(I) xor MATRIX_A;
            end if;
         end loop;
      end if;

      y := G.s(G.p);
      y := y xor Shift_Right(y, 11);
      y := y xor (Shift_Left(y, 7) and 16#9d2c5680#);
      y := y xor (Shift_Left(y, 15) and 16#efc60000#);
      y := y xor Shift_Right(y, 18);
      G.p := G.p + 1;
      return y;

   end Generate;

end PRNG_Zoo.MT;
