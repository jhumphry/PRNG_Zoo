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

with Ada.Unchecked_Deallocation;

with PRNG_Zoo;
use PRNG_Zoo;

with Parse_Args;
use Parse_Args;

with Parse_Args.Generic_Options;
with Parse_Args.Generic_Indefinite_Options;
with Parse_Args.Split_CSV;

package Common_CLI_Options is

   package U64_Options is new Parse_Args.Generic_Options(Element => PRNG_Zoo.U64,
                                                        Fallback_Default => 0,
                                                        Value => U64'Value,
                                                        Image => U64'Image
                                                       );

   procedure Free_U64_Array is
     new Ada.Unchecked_Deallocation(Object => U64_array,
                                    Name => U64_array_access);

   function Split_U64_Array is new Parse_Args.Split_CSV(Element => U64,
                                               Element_Array => U64_array,
                                               Element_Array_Access => U64_array_access,
                                               Value => U64'Value
                                              );

   function U64_Array_Image(Arg : U64_array_access) return String is
      ("<U64 array of length: " & Integer'Image(Arg.all'Length) & ">");

   package U64_array_Options is new Parse_Args.Generic_Indefinite_Options(Element => U64_array,
                                                                         Element_Access => U64_array_access,
                                                                         Value => Split_U64_Array,
                                                                         Image => U64_Array_Image,
                                                                         Free_Element => Free_U64_Array
                                                                        );

end Common_CLI_Options;
