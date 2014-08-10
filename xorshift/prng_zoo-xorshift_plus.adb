package body PRNG_Zoo.xorshift_plus is

   --------------
   -- Strength --
   --------------

   function Strength (G: in xorshift128_plus) return PRNG_Strength is
   begin
     return Medium;
   end Strength;

   -----------
   -- Reset --
   -----------

   procedure Reset (G: in out xorshift128_plus; S: in U64) is
   begin
      -- This is an ad-hoc expedient for now.

      G.s0 := S;
      G.s1 := (not S) xor Shift_Left(S, 17) xor Shift_Right(S, 23);
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out xorshift128_plus) return U64 is
      t0: constant U64 := G.s1;
      t1: U64 := G.s0;
   begin
      G.s0 := t0;
      t1 := t1 xor Shift_Left(t1, 23);
      G.s1 := (t1 xor t0 xor Shift_Right(t1, 17) xor Shift_Right(t0, 23));
      return G.s1 + t0;
   end Generate;

end PRNG_Zoo.xorshift_plus;
