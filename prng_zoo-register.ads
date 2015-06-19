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

with Ada.Tags;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

package PRNG_Zoo.Register is

   use all type Ada.Strings.Unbounded.Unbounded_String;
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   subtype PRNG_Description is Ada.Strings.Unbounded.Unbounded_String;

   package Parameters_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps(Key_Type     => String,
                                                Element_Type => String);
   type PRNG_Parameters_Map is new PRNG_Parameters with
      record
         Params : Parameters_Maps.Map;
      end record;
   function Contains(Container : PRNG_Parameters_Map; Key : String) return Boolean;
   function Parameter(Container : PRNG_Parameters_Map; Key : String) return String;
   function Parameter(Container : PRNG_Parameters_Map; Key : String) return U64;
   function Split_Parameters(S : String) return PRNG_Parameters_Map;

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

   type PRNG_Spec is
      record
         Name : Unbounded_String;
         Params : Unbounded_String;
      end record;
   function Name(Spec : PRNG_Spec) return String is
      (Ada.Strings.Unbounded.To_String(Spec.Name));

    function Make_PRNG(Spec : PRNG_Spec) return PRNG'Class;

   procedure Display_Register;

end PRNG_Zoo.Register;
