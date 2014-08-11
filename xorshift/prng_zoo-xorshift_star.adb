--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package body PRNG_Zoo.xorshift_star is


   -----------
   -- Reset --
   -----------

   procedure Reset (G : in out xorshift64_star; S : in U64) is
   begin
      G.s := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G : in out xorshift64_star) return U64 is
   begin
      G.s := G.s xor Shift_Right(G.s, 12);
      G.s := G.s xor Shift_Left(G.s, 25);
      G.s := G.s xor Shift_Right(G.s, 17);
      return G.s * M32;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (G : in out xorshift1024_star; S : in U64) is
      G64 : xorshift64_star;
   begin
      Reset(G64, S);
      for I in p16'Range loop
         G.s(I) := Generate(G64);
      end loop;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G : in out xorshift1024_star) return U64 is
      s0, s1: U64;
   begin
      s0 := G.s(G.p);
      G.p := G.p + 1;
      s1 := G.s(G.p);

      s1 := s1 xor Shift_Left(s1, 31);
      s1 := s1 xor Shift_Right(s1, 11);
      s0 := s0 xor Shift_Right(s1, 30);
      G.s(G.p) := S0 xor S1;

      return G.s(G.p) * M8;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (G : in out xorshift4096_star; S : in U64) is
      G64 : xorshift64_star;
   begin
      Reset(G64, S);
      for I in p64'Range loop
         G.s(I) := Generate(G64);
      end loop;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G : in out xorshift4096_star) return U64 is
      s0, s1: U64;
   begin
      s0 := G.s(G.p);
      G.p := G.p + 1;
      s1 := G.s(G.p);

      s1 := s1 xor Shift_Left(s1, 25);
      s1 := s1 xor Shift_Right(s1, 3);
      s0 := s0 xor Shift_Right(s1, 49);
      G.s(G.p) := S0 xor S1;

      return G.s(G.p) * M2;
   end Generate;

end PRNG_Zoo.xorshift_star;
