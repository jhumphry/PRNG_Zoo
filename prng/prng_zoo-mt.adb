--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

-- Includes material derived from mt19937ar.c:

--     Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
--     All rights reserved.

-- Includes material derived from mt19937-64.c:

--     Copyright (C) 2004, Makoto Matsumoto and Takuji Nishimura,
--     All rights reserved.

-- The copyright conditions notice of both follows:
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

-- Includes material derived from tinymt64.h and tinymt64.c:

--    Copyright (C) 2011, 2013 Mutsuo Saito, Makoto Matsumoto,
--    Hiroshima University and The University of Tokyo.

--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions are
--     met:
--
--         * Redistributions of source code must retain the above copyright
--           notice, this list of conditions and the following disclaimer.
--         * Redistributions in binary form must reproduce the above
--           copyright notice, this list of conditions and the following
--           disclaimer in the documentation and/or other materials provided
--           with the distribution.
--         * Neither the name of the Hiroshima University nor the names of
--           its contributors may be used to endorse or promote products
--           derived from this software without specific prior written
--           permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--     A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--     OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--     OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package body PRNG_Zoo.MT is

   -- Mersenne Twister constants
   N : constant := 624;
   M : constant := 397;
   MATRIX_A : constant U32 := 16#9908B0DF#;
   UPPER_MASK : constant U32 := 16#80000000#;
   LOWER_MASK : constant U32 := 16#7FFFFFFF#;

   -- Mersenne Twister 64-bit constants
   NN : constant := 312;
   MM : constant := 156;
   MATRIX_A_64 : constant U64 := 16#B5026F5AA96619E9#;
   UM : constant U64 := 16#FFFFFFFF80000000#;
   LM : constant U64 := 16#7FFFFFFF#;

   -- TinyMT 64-bit constants
   TINYMT64_MEXP : constant := 127;
   TINYMT64_SH0 : constant := 12;
   TINYMT64_SH1 : constant := 11;
   TINYMT64_SH8 : constant := 8;
   TINYMT64_MASK : constant U64 := 16#7fffffffffffffff#;
   MIN_LOOP : constant := 8;

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


   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out MT19937_64; S: in U64) is
   begin
      G.s(0) := S;
      G.p := 0;
      For I in MT_Index_64 range 1..MT_Index_64(NN-1) loop
         G.s(I) := 6364136223846793005 * (G.s(I-1) xor Shift_Right(G.s(I-1), 62))
           + U64(I);
      end loop;
   end Reset;

   procedure Reset (G: in out MT19937_64; S: in U64_array) is
      key : U64_array(0..S'Length-1) := S;
      i : MT_Index_64;
      j : Integer;
   begin
      Reset(G, U64(19650218));

      i := 1;
      j := 0;

      for k in reverse 1..(if NN > key'Length then NN else key'Length) loop
         G.s(i) := (G.s(i) xor ((G.s(i-1) xor Shift_Right(G.s(i-1), 62)) * 3935559000370003845)) +
           key(j) + U64(j);
         i := i + 1;
         j := (j + 1) mod key'Length;
         if i=0 then
            G.s(0) := G.s(MT_Index_64(NN-1));
            i := 1;
         end if;
      end loop;

      for k in reverse 1..MT_Index_64(NN-1) loop
         G.s(i) := (G.s(i) xor ((G.s(i-1) xor Shift_Right(G.s(i-1), 62)) * 2862933555777941757))
           - U64(i);
         i := i + 1;
         if i=0 then
            G.s(0) := G.s(MT_Index_64(NN-1));
            i := 1;
         end if;
      end loop;

      G.s(0) := Shift_Left(U64(1),63);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out MT19937_64) return U64 is
         x : U64;
   begin
      if G.p = 0 then
         for I in MT_Index_64'Range loop
            x := (G.s(I) and UM) or (G.s(I+1) and LM);
            G.s(I) := G.s(I+MM) xor Shift_Right(x, 1);
            if (x mod 2) = 1 then
               G.s(I) := G.s(I) xor MATRIX_A_64;
            end if;
         end loop;
      end if;

      x := G.s(G.p);
      x := x xor (Shift_Right(x, 29) and 16#5555555555555555#);
      x := x xor (Shift_Left(x, 17) and 16#71D67FFFEDA60000#);
      x := x xor (Shift_Left(x, 37) and 16#FFF7EEE000000000#);
      x := x xor Shift_Right(x, 43);
      G.p := G.p + 1;
      return x;

   end Generate;

   -----------
   -- Reset --
   -----------

   procedure period_certification(random : in out TinyMT_64) is
   begin
      if ((random.status(0) and TINYMT64_MASK) = 0) and (random.status(1) = 0) then
         random.status(0) := Character'Pos('T');
         random.status(1) := Character'Pos('M');
      end if;
   end period_certification;

   procedure Reset(G: in out TinyMT_64; S: in U64) is
   begin
      G.status(0) := S xor Shift_Left(U64(G.mat1), 32);
      G.status(1) := U64(G.mat2) xor G.tmat;
      for I in U64 range 1..(MIN_LOOP-1) loop
         G.status(I and 1) := G.status(I and 1) xor
           (I + U64'(6364136223846793005) *
            (G.status((I-1) and 1) xor Shift_Right(G.status((I-1) and 1), 62)));
      end loop;
      period_certification(G);
   end Reset;

   procedure Reset(G: in out TinyMT_64; S: in U64_array) is
      function ini_func1(x : U64) return U64 is
        ((x xor Shift_Right(x, 59)) * U64(2173292883993));

      function ini_func2(x : U64) return U64 is
        ((x xor Shift_Right(x, 59)) * U64(58885565329898161));

      lag : constant := 1;
      mid : constant := 1;
      size : constant := 4;
      type i_type is mod(size);
      i : i_type;
      count : Integer;
      r : U64;
      st : array (i_type) of U64 := (0, U64(G.mat1), U64(G.mat2), G.tmat);

   begin
      count := Integer'Max(S'Length + 1, MIN_LOOP);
      r := ini_func1(st(0) xor st(mid) xor st(i_type(size-1)));
      st(mid) := st(mid) + r;
      r := r + S'Length;
      st(mid + lag) := st(mid + lag) + r;
      st(0) := r;
      count := count - 1;

      i := 1;
      for j in 0..(Integer'Min(count,S'Length)-1) loop
         r := ini_func1(st(i) xor st(i + mid) xor st(i + i_type(size-1)));
         st(i + mid) := st(i + mid) + r;
         r := r + S(S'First + j) + U64(i);
         st(i+mid+lag) := st(i+mid+lag) + r;
         st(i) := r;
         i := (i + 1);
      end loop;

      for j in S'Length .. (count-1) loop
         r := ini_func1(st(i) xor st(i + mid) xor st(i + i_type(size-1)));
         st(i + mid) := st(i + mid) + r;
         r := r + U64(i);
         st(i+mid+lag) := st(i+mid+lag) + r;
         st(i) := r;
         i := (i + 1);
      end loop;

      for j in 0..(size - 1) loop
         r := ini_func2(st(i) + st(i + mid) + st(i + i_type(size-1)));
         st(i + mid) := st(i + mid) xor r;
         r := r - U64(i);
         st(i+mid+lag) := st(i+mid+lag) xor r;
         st(i) := r;
         i := (i + 1);
      end loop;

      G.status(0) := st(0) xor st(1);
      G.status(1) := st(2) xor st(3);
      period_certification(G);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate(G: in out TinyMT_64) return U64 is
      procedure tinymt64_next_state(random : in out TinyMT_64) with inline is
         x : U64;
      begin
         random.status(0) := random.status(0) and TINYMT64_MASK;
         x := random.status(0) xor random.status(1);
         x := x xor Shift_Left(x, TINYMT64_SH0);
         x := x xor Shift_Right(x, 32);
         x := x xor Shift_Left(x, 32);
         x := x xor Shift_Left(x, TINYMT64_SH1);
         random.status(0) := random.status(1);
         random.status(1) := x;
         random.status(0) := random.status(0) xor (if (x and 1)=1 then U64(random.mat1) else 0);
         random.status(1) := random.status(1) xor (if (x and 1)=1 then Shift_Left(U64(random.mat2),32) else 0);
      end tinymt64_next_state;

      function tinymt64_temper(random : in TinyMT_64) return U64 with inline is
         x : U64;
      begin
         x := random.status(0) + random.status(1);
         x := x xor Shift_Right(random.status(0), TINYMT64_SH8);
         x := x xor (if (x and 1)=1 then random.tmat else 0);
         return x;
      end tinymt64_temper;

   begin
      tinymt64_next_state(G);
      return tinymt64_temper(G);
   end Generate;

end PRNG_Zoo.MT;
