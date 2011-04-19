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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with OAuth;
with SHA;
with SHA.Strings;
with HMAC_SHA;

use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Test_Oauth is
   Params : OAuth.Parameter_List.List;
   Digest_Val : SHA.Digest;
   Base_Str : Unbounded_String;
   Ctx : HMAC_SHA.HMAC_Context;
begin
   Params := OAuth.Create_Parameter_List
     (Consumer_Key => "dpf43f3p2l4k3l03",
      Token        => "nnch734d00sl2jdk",
      Signature_Method => "HMAC-SHA1",
      Timestamp    => "1191242096",
      Nonce        => "kllo9940pd9333jh",
      Version      => "1.0");

   OAuth.Parameter_List.Append
     (Params,
       (To_Unbounded_String ("file"),
        To_Unbounded_String ("vacation.jpg")));
   OAuth.Parameter_List.Append
     (Params,
       (To_Unbounded_String ("size"),
        To_Unbounded_String ("original")));
   Base_Str := To_Unbounded_String (OAuth.Create_Base_String
     ("GET", "http://photos.example.net/photos", Params));

   HMAC_SHA.Initialize (Key => "kd94hf93k423kf44&pfkkdhi9sl3r4s00",
                        Context => Ctx);
   HMAC_SHA.Add (Message => To_String (Base_Str),
                 Context => Ctx);
   HMAC_SHA.Finalize (Result => Digest_Val,
                      Context => Ctx);
   Put_Line (String (SHA.Strings.B64_From_SHA (Digest_Val)));

   Put_Line (OAuth.Timestamp);

   Put_Line (Oauth.Nonce ("A", "B", "C"));

end Test_Oauth;
