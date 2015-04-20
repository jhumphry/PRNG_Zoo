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

package body PRNG_Zoo is

   --------------
   -- Generate --
   --------------

   function Generate (G: in out PRNG_32Only) return U64 is
      R1 : constant U32 := Generate(PRNG_32Only'Class(G));
      R2 : constant U32 := Generate(PRNG_32Only'Class(G));
   begin
      return Shift_Left(U64(R1), 32) or U64(R2);
   end Generate;

   ---------------------
   -- Generate_Padded --
   ---------------------

   function Generate_Padded (G: in out PRNG_32Only) return U64 is
   begin
      return U64(U32'(Generate(PRNG_32Only'Class(G))));
   end Generate_Padded;

   ---------------------
   -- Generate_Padded --
   ---------------------

   function Generate_Padded (G: in out PRNG_64Only) return U64 is
   begin
      return U64'(Generate(PRNG_64Only'Class(G)));
   end Generate_Padded;

   --------------
   -- Generate --
   --------------

   function Generate (G: in out PRNG_64Only) return U32 is
   begin
      return U32(U64'(Generate(PRNG_64Only'Class(G))) and 16#FFFFFFFF#);
   end Generate;

   -----------
   -- Reset --
   -----------

   procedure Reset(G: in out Dispatcher; S: in U64) is
   begin
      Reset(G.IG.all, S);
   end Reset;


end PRNG_Zoo;
