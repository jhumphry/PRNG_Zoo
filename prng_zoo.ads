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

with Interfaces;
use all type Interfaces.Unsigned_64;
use all type Interfaces.Unsigned_32;

with Ada.Tags.Generic_Dispatching_Constructor;

package PRNG_Zoo is

   subtype U64 is Interfaces.Unsigned_64;
   type U64_array is array (Integer range <>) of U64;
   type U64_array_access is access U64_array;
   subtype U64_Nonzero is
     Interfaces.Unsigned_64 range 1..Interfaces.Unsigned_64'Last - 1;

   subtype U32 is Interfaces.Unsigned_32;
   type U32_array is array (Integer range <>) of U32;
   type U32_array_access is access U32_array;
   subtype U32_Nonzero is
     Interfaces.Unsigned_32 range 1..Interfaces.Unsigned_32'Last - 1;

   type PRNG_Strength is (Crypto, High, Medium, Low, Dummy);

   type PRNG_Parameters is tagged null record;
   function Contains(Container : PRNG_Parameters;
                     Key : String) return Boolean is
     (False);
   function Parameter(Container : PRNG_Parameters
                      ; Key : String) return String is
     (raise Constraint_Error);
   function Parameter(Container : PRNG_Parameters;
                      Key : String;
                      Default : String) return String is
     (if Contains(Container, Key) then Parameter(Container, Key) else Default);
   function Parameter(Container : PRNG_Parameters;
                      Key : String) return U64 is
     (raise Constraint_Error);
   function Parameter(Container : PRNG_Parameters;
                      Key : String;
                      Default : U64) return U64 is
     (if Contains(Container, Key) then Parameter(Container, Key) else Default);
   function Parameter(Container : PRNG_Parameters;
                      Key : String) return Integer is
     (raise Constraint_Error);
   function Parameter(Container : PRNG_Parameters;
                      Key : String;
                      Default : Integer) return Integer is
     (if Contains(Container, Key) then Parameter(Container, Key) else Default);
   No_Parameters : aliased constant PRNG_Parameters := (others => <>);
   Invalid_Parameters : exception;

   type PRNG is interface;
   function Strength(G: in PRNG) return PRNG_Strength is abstract;
   function Width(G: in PRNG) return Positive is abstract;
   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return PRNG is abstract;
   procedure Reset(G: in out PRNG; S: in U64) is abstract;
   function Generate(G: in out PRNG) return U64 is abstract;
   function Generate_Padded(G: in out PRNG) return U64 is abstract;
   function Generate(G: in out PRNG) return U32 is abstract;

   function PRNG_Constructor is new
     Ada.Tags.Generic_Dispatching_Constructor(T => PRNG,
                                              Parameters => PRNG_Parameters'Class,
                                              Constructor => Constructor);

   type PRNG_Seed_From_Array is interface;
   procedure Reset(G: in out PRNG_Seed_From_Array; S: in U64_array) is abstract;

   type PRNG_32Only is abstract new PRNG with null record;
   function Width(G: in PRNG_32Only) return Positive is (32);
   function Generate(G: in out PRNG_32Only) return U64 with inline;
   function Generate_Padded(G: in out PRNG_32Only) return U64 with inline;

   -- While it would be purer to implement Generate_Padded as a simple renaming,
   -- this would not be inherited and would need repeating for each derived
   -- type. In speed-optimised uses the simple inline function should be
   -- optimised away.
   type PRNG_64Only is abstract new PRNG with null record;
   function Width(G: in PRNG_64Only) return Positive is (64);
   function Generate_Padded(G: in out PRNG_64Only) return U64 with inline;
   function Generate(G: in out PRNG_64Only) return U32 with inline;

   type PRNG_Ptr is access all PRNG'Class;

   -- The Dispatcher type exists so that other routines can be written as
   -- generics with compile-type polymorphism (for optimum performance in actual
   -- simulations) while still allowing for run-time polymorphism for tests of
   -- statistical quality and relative speed.

   type Dispatcher(IG : access PRNG'Class) is new PRNG with null record;
   function Strength(G: in Dispatcher) return PRNG_Strength is
     (Strength(G.IG.all));
   function Width(G: in Dispatcher) return Positive is
     (Width(G.IG.all));
   function Constructor(Params : not null access PRNG_Parameters'Class)
                        return Dispatcher is
     (if true then raise Program_Error else raise Program_Error);
   procedure Reset(G: in out Dispatcher; S: in U64);
   function Generate(G: in out Dispatcher) return U64 is
     (Generate(G.IG.all)) with inline;
   function Generate_Padded(G: in out Dispatcher) return U64 is
     (Generate_Padded(G.IG.all)) with inline;
   function Generate(G: in out Dispatcher) return U32 is
     (Generate(G.IG.all)) with inline;

   scale_U31 : constant := 4.65661_28730_77392_57812_50000E-10;
   scale_U32 : constant := 2.32830_64365_38696_28906_25000E-10;
   scale_U64 : constant := 5.42101_08624_27522_17003_72640_04349_70855_71289_06250E-20;

end PRNG_Zoo;
