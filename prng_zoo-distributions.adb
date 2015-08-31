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

package body PRNG_Zoo.Distributions is

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Uniform01; G : in out PRNG'Class) return Float_Type is
      X : constant U64 := Generate_Padded(G);
   begin
      return Float_Type(X) * scale;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Uniform; G : in out PRNG'Class) return Float_Type is
      X : constant U64 := Generate_Padded(G);
   begin
      return D.a + D.b * Float_Type(X) * scale;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Normal_12_6; G : in out PRNG'Class) return Float_Type is
      Result : Float_Type := 0.0;
      X : U64;
   begin
      for I in 1..12 loop
         X := Generate_Padded(G);
         Result := Result + Float_Type(X) * scale;
      end loop;
      return Result - 6.0;
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset (D: in out Normal_Box_Mueller) is
   begin
      D.Loaded := False;
   end Reset;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Normal_Box_Mueller; G : in out PRNG'Class) return Float_Type is
      t1, t2 : Float_Type;
      s : Float_Type;
      ss : Float_Type;
      X, Y : U64;
   begin
      if D.Loaded then
         D.Loaded := False;
         return D.r2;
      end if;

      loop
         X := Generate_Padded(G);
         Y := Generate_Padded(G);
         t1 := 2.0 * Float_Type(X) * scale - 1.0;
         t2 := 2.0 * Float_Type(Y) * scale - 1.0;
         s := t1*t1 + t2*t2;
         exit when s <= 1.0;
      end loop;
      ss := Sqrt(-2.0 * Log(s + Float_Type'Model_Small) / s);
      D.Loaded := True;
      D.r2 := t2 * ss;
      return t1 * ss;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Normal_Monty_Python; G : in out PRNG'Class) return Float_Type is

      a : constant Float_Type := 1.17741_00225_15474_69101; -- sqrt(log(4))
      b : constant Float_Type := 2.50662_82746_31000_50240; -- sqrt(2*pi)
      s : constant Float_Type := 0.88579_13443_79721_11687; -- a / (b-a)
      b_recip : constant Float_Type := 0.39894_22804_01432_67793; -- 1/b
      log_norm_c : constant Float_Type := -0.22579_13526_44727_43236; -- log(2/sqrt(2*pi))

      v : constant U64 := Generate_Padded(G);
      sign : constant Float_Type := (if (v and 1) = 1 then +1.0 else -1.0);
      x : Float_Type := Float_Type(v) * scale * b;
      y : Float_Type;

      TX, TY : U64;

   begin
      if x < a then
         return x * sign;
      end if;

      TX := Generate_Padded(G);
      y := Float_Type(TX) * scale * b_recip;
      if Log(y) < (log_norm_c - 0.5 * x * x) then
         return x * sign;
      end if;

      x := s*(b-x);
      y := b_recip + ( b_recip - y) / s;
      if Log(y) <  (log_norm_c - 0.5 * x * x) then
         return x * sign;
      end if;

      loop
         TX:= Generate_Padded(G);
         TY:= Generate_Padded(G);
         x := -Log(Float_Type(TX) * scale) * b_recip;
         y := -Log(Float_Type(TY) * scale);
         exit when y+y > x*x;
      end loop;

      return (b + x) * sign;

   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Exponential; G : in out PRNG'Class) return Float_Type is
      X : constant U64 := Generate_Padded(G);
   begin
      return -D.theta * Log(Float_Type(X) * scale + Float_Type'Model_Small);
   end Generate;

end PRNG_Zoo.Distributions;
