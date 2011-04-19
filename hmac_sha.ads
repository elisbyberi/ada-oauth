--
-- Copyright (c) 2011 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Interfaces;
with SHA.Process_Data;
with SHA;

package HMAC_SHA is
   type HMAC_Context is private;

   procedure Initialize (Key : String; Context : out HMAC_Context);
   procedure Add (Message : String; Context : in out HMAC_Context);
   procedure Finalize (Result  :    out SHA.Digest;
                       Context : in out HMAC_Context);
private
   type U32_Array is array(1..16) of Interfaces.Unsigned_32;

   type HMAC_Context is record
      Ipad_Key : U32_Array;
      Opad_Key : U32_Array;
      Ipad_Context : SHA.Process_Data.Context;
   end record;
end HMAC_SHA;
