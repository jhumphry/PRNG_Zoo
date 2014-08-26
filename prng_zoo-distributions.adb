--
-- PRNG Zoo
-- Copyright 2014 James Humphry
--

package body PRNG_Zoo.Distributions is

   scale_U64 : constant := 5.42101_08624_27522_17003_72640_04349_70855_71289_06250E-20;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Uniform01; G : in out P) return Float_Type is
   begin
      return Float_Type(Generate(G)) * scale_U64;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Uniform; G : in out P) return Float_Type is
      a : Float_Type := D.a;
      b : Float_Type := D.b;
   begin
      return a + b * Float_Type(Generate(G)) * scale_U64;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate (D: in out Normal_12_6; G : in out P) return Float_Type is
      Result : Float_Type := 0.0;
   begin
      for I in 1..12 loop
         Result := Result + Float_Type(Generate(G)) * scale_U64;
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
         t1 := 2.0 * Float_Type(Generate(G)) * scale_U64 - 1.0;
         t2 := 2.0 * Float_Type(Generate(G)) * scale_U64 - 1.0;
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

   function Generate (D: in out Exponential; G : in out P) return Float_Type is
   begin
      return -D.theta * log(Float_Type(Generate(G)) * scale_U64 + Float_Type'Model_Small);
   end Generate;

end PRNG_Zoo.Distributions;