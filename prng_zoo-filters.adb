--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package body PRNG_Zoo.Filters is

   --------------
   -- Generate --
   --------------

   function Generate (G: in out Split_32) return U32 is
      V : U64;
   begin
      if G.Loaded then
         G.Loaded := False;
         return G.Next_Value;
      else
         V := G.IG.Generate;
         G.Loaded := True;
         G.Next_Value := U32(Shift_Right(V,32));
         return U32(V and 16#FFFFFFFF#);
      end if;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out Bit_Reverse) return U64 is
     V : U64;
   begin
      V := G.IG.Generate;
      V := (Shift_Right(V, 1) and 16#5555555555555555#)
        or Shift_Left((V and 16#5555555555555555#), 1);
      V := (Shift_Right(V, 2) and 16#3333333333333333#)
        or Shift_Left((V and 16#3333333333333333#), 2);
      V := (Shift_Right(V, 4) and 16#0F0F0F0F0F0F0F0F#)
        or Shift_Left((V and 16#0F0F0F0F0F0F0F0F#), 4);
      V := (Shift_Right(V, 8) and 16#00FF00FF00FF00FF#)
        or Shift_Left((V and 16#00FF00FF00FF00FF#), 8);
      V := (Shift_Right(V, 16) and 16#0000FFFF0000FFFF#)
        or Shift_Left((V and 16#0000FFFF0000FFFF#), 16);
      V := Shift_Right(V, 32)  or Shift_Left(V, 32);
      return V;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out Bit_Reverse) return U32 is
      V : U32;
   begin
      V := G.IG.Generate;
      V := (Shift_Right(V, 1) and 16#55555555#)
        or Shift_Left((V and 16#55555555#), 1);
      V := (Shift_Right(V, 2) and 16#33333333#)
        or Shift_Left((V and 16#33333333#), 2);
      V := (Shift_Right(V, 4) and 16#0F0F0F0F#)
        or Shift_Left((V and 16#0F0F0F0F#), 4);
      V := (Shift_Right(V, 8) and 16#00FF00FF#)
        or Shift_Left((V and 16#00FF00FF#), 8);
      V := Shift_Right(V, 16)  or Shift_Left(V, 16);
      return V;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset(G: in out Incrementer; S: in U64) is
   begin
      G.S := S;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate(G: in out Incrementer) return U64 is
   begin
      G.S := G.S + G.Incr;
      return G.S;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate(G: in out Incrementer) return U32 is
   begin
      G.S := G.S + G.Incr;
      return U32(G.S and 16#FFFFFFFF#);
   end Generate;

end PRNG_Zoo.Filters;
