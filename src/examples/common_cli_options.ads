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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with PRNG_Zoo, PRNG_Zoo.Register;
use PRNG_Zoo;
use type PRNG_Zoo.Register.PRNG_Spec;

with Parse_Args;
use Parse_Args;

with Parse_Args.Generic_Options;
with Parse_Args.Generic_Discrete_Array_Options;
with Parse_Args.Split_CSV;

package Common_CLI_Options is

   package PRNG_Spec_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists(PRNG_Zoo.Register.PRNG_Spec);

   package U64_Options is new Parse_Args.Generic_Options(Element => PRNG_Zoo.U64,
                                                        Fallback_Default => 0,
                                                        Value => U64'Value,
                                                        Image => U64'Image
                                                       );

   package U64_array_Options is
     new Parse_Args.Generic_Discrete_Array_Options(Element => U64,
                                                   Element_Array => U64_array,
                                                   Element_Array_Access => U64_array_access
                                                  );

end Common_CLI_Options;
