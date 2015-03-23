--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with Ada.Tags;
with Ada.Strings.Bounded;
with Ada.Containers.Indefinite_Ordered_Maps;

package PRNG_Zoo.Register is

   package PRNG_Descriptions is
     new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 60);
   use all type PRNG_Descriptions.Bounded_String;
   subtype PRNG_Description is PRNG_Descriptions.Bounded_String;

   type PRNG_Details is
      record
         Tag : Ada.Tags.Tag;
         Params : not null access PRNG_Parameters'Class;
         Description : PRNG_Description;
      end record;

   package PRNG_Registries is
     new Ada.Containers.Indefinite_Ordered_Maps(Key_Type     => String,
                                                Element_Type => PRNG_Details);
   subtype Register_Cursor is PRNG_Registries.Cursor;
   Register : PRNG_Registries.Map;

   function PRNG_Exists(Name : String) return Boolean is
     (Register.Contains(Name));

   procedure PRNG_Column_Widths(Names, Descriptions : in out Natural);

   function Make_PRNG(Name : String) return PRNG'Class is
      (PRNG_Constructor(Register(Name).Tag, Register(Name).Params));

   procedure Display_Register;

end PRNG_Zoo.Register;
