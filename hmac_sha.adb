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
with Utils;
with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;
with SHA.Process_Data;
pragma Elaborate_All (Sha.Process_Data);

package body HMAC_SHA is
   use type Interfaces.Unsigned_32;

   Ipad : constant := 16#36363636#;
   Opad : constant := 16#5C5C5C5C#;

   procedure Initialize (Key : String; Context : out HMAC_Context) is
      Key_Array  : U32_Array := (others => 0);
      Key_Digest : SHA.Digest;
      K_Index : Positive;
   begin
      if Key'Last - Key'First + 1 > 64 then
         Key_Digest := SHA.Process_Data.Digest_A_String (Key);
         K_Index := Key_Array'First;
         for I in Key_Digest'Range loop
            Key_Array (K_Index) := Key_Digest (I);
            K_Index := K_Index + 1;
         end loop;
      else
         declare
            Place : Natural := Key'First;
            Key_Index : Natural := 1;
         begin
            loop
               exit when Place > Key'Last or Key_Index > Key_Array'Last;
               if Key'Last - Place >= 3 then
                  Key_Array (Key_Index) :=
                    Utils.String_to_U32 (Key (Place .. Place + 3));
               else
                  Key_Array (Key_Index) :=
                    Utils.String_to_U32 (Key (Place .. Key'Last));
               end if;

               Place := Place + 4;
               Key_Index := Key_Index + 1;
            end loop;
         end;
      end if;

      for I in Key_Array'Range loop
         Context.Ipad_Key (I) := Ipad xor Key_Array(I);
         Context.Opad_Key (I) := Opad xor Key_Array(I);
      end loop;

      SHA.Process_Data.Initialize (Context.Ipad_Context);
      for I in Context.Ipad_Key'Range loop
         SHA.Process_Data.Add (SHA.Process_Data.Long
           (Context.Ipad_Key (I)), Context.Ipad_Context);
      end loop;
   end Initialize;

   procedure Add (Message : String; Context : in out HMAC_Context) is
   begin
      for I in Message'Range loop
         SHA.Process_Data.Add (SHA.Process_Data.Byte
           (Character'Pos (Message (I))), Context.Ipad_Context);
      end loop;
   end Add;

   procedure Finalize (Result  :    out SHA.Digest;
                       Context : in out HMAC_Context) is
      Opad_Context : SHA.Process_Data.Context;
      Ipad_Result  : SHA.Digest;
   begin
      SHA.Process_Data.Finalize (Ipad_Result, Context.Ipad_Context);

      SHA.Process_Data.Initialize (Opad_Context);
      for I in Context.Opad_Key'Range loop
         SHA.Process_Data.Add (SHA.Process_Data.Long
           (Context.Opad_Key (I)), Opad_Context);
      end loop;
      for I in Ipad_Result'Range loop
         SHA.Process_Data.Add (SHA.Process_Data.Long
           (Ipad_Result (I)), Opad_Context);
      end loop;
      SHA.Process_Data.Finalize (Result, Opad_Context);
   end Finalize;
end HMAC_SHA;
