--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package body PRNG_Zoo.Distributions is

   scale_U64 : constant Float_Type := 5.42101_08624_27522_17003_72640_04349_70855_71289_06250E-20;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Uniform01; G : in out P) return Float_Type is
   begin
      return Float_Type(U64'(Generate(G))) * scale_U64;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Uniform; G : in out P) return Float_Type is
      a : Float_Type := D.a;
      b : Float_Type := D.b;
   begin
      return a + b * Float_Type(U64'(Generate(G))) * scale_U64;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Normal_12_6; G : in out P) return Float_Type is
      Result : Float_Type := 0.0;
   begin
      for I in 1..12 loop
         Result := Result + Float_Type(U64'(Generate(G))) * scale_U64;
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

   function Generate (D: in out Normal_Box_Mueller; G : in out P) return Float_Type is
      t1, t2 : Float_Type;
      s : Float_Type;
      ss : Float_Type;
   begin
      if D.Loaded then
         D.Loaded := False;
         return D.r2;
      end if;

      loop
         t1 := 2.0 * Float_Type(U64'(Generate(G))) * scale_U64 - 1.0;
         t2 := 2.0 * Float_Type(U64'(Generate(G))) * scale_U64 - 1.0;
         s := t1*t1 + t2*t2;
         exit when s <= 1.0;
      end loop;
      ss := sqrt(-2.0 * log(s + Float_Type'Model_Small) / s);
      D.Loaded := True;
      D.r2 := t2 * ss;
      return t1 * ss;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Normal_Monty_Python; G : in out P) return Float_Type is

      a : constant Float_Type := 1.17741_00225_15474_69101; -- sqrt(log(4))
      b : constant Float_Type := 2.50662_82746_31000_50240; -- sqrt(2*pi)
      s : constant Float_Type := 0.88579_13443_79721_11687; -- a / (b-a)
      b_recip : constant Float_Type := 0.39894_22804_01432_67793; -- 1/b
      log_norm_c : constant Float_Type := -0.22579_13526_44727_43236; -- log(2/sqrt(2*pi))

      v : U64 := Generate(G);
      sign : Float_Type := (if (v and 1) = 1 then +1.0 else -1.0);
      x : Float_Type := Float_Type(v) * scale_U64 * b;
      y : Float_Type;

   begin
      if x < a then
         return x * sign;
      end if;

      y := Float_Type(U64'(Generate(G))) * scale_U64 * b_recip;
      if log(y) < (log_norm_c - 0.5 * x * x) then
         return x * sign;
      end if;

      x := s*(b-x);
      y := b_recip + ( b_recip - y) / s;
      if log(y) <  (log_norm_c - 0.5 * x * x) then
         return x * sign;
      end if;

      loop
         x := -log(Float_Type(U64'(Generate(G))) * scale_U64) * b_recip;
         y := -log(Float_Type(U64'(Generate(G))) * scale_U64);
         exit when y+y > x*x;
      end loop;

      return (b + x) * sign;

   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Exponential; G : in out P) return Float_Type is
   begin
      return -D.theta * log(Float_Type(U64'(Generate(G))) * scale_U64 + Float_Type'Model_Small);
   end Generate;

end PRNG_Zoo.Distributions;
