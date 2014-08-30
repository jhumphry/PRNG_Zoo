--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

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
