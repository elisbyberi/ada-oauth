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

with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces;
with SHA;
with SHA.Process_Data;
with SHA.Strings;

package body OAuth is
   function "+"(Str : String) return Unbounded_String
     renames To_Unbounded_String;

   function Compare_Pairs (Left, Right : String_Pair) return Boolean is
   begin
      return Left.Key < Right.Key;
   end Compare_Pairs;

   function URL_Encode (Str : String) return String is
      Reserved : constant String := ":/?#[]@!$&'()*+,;= ";
      function Is_Reserved (Char : Character) return Boolean is
      begin
         for I in Reserved'Range loop
            if Char = Reserved (I) then
               return True;
            end if;
         end loop;

         return False;
      end Is_Reserved;

      function Escape (Char : Character) return String is
         use Interfaces;

         Val  : Unsigned_8 := Unsigned_8 (Character'Pos (Char));
         High : Unsigned_8 := Shift_Right (Val, 4);
         Low  : Unsigned_8 := Val and 16#0F#;

         Hex_Digits : String := "0123456789ABCDEF";
      begin
         return "%" & Hex_Digits (1 + Integer (High)) &
           Hex_Digits (1 + Integer (Low));
      end Escape;

      Result : Unbounded_String := Null_Unbounded_String;

   begin
      for I in Str'Range loop
         if Is_Reserved (Str (I)) then
            Append (Result, Escape (Str (I)));
         else
            Append (Result, Str (I));
         end if;
      end loop;
      return To_String (Result);
   end URL_Encode;

   function Create_Parameter_List (Key_1   : String;
                                   Value_1 : String;
                                   Key_2   : String;
                                   Value_2 : String)
     return Parameter_List.List is
      use Parameter_List;

      Params : List := Empty_List;
   begin
      Append (Params, (Key => +Key_1, Value => +Value_1));
      Append (Params, (Key => +Key_2, Value => +Value_2));

      return Params;
   end Create_Parameter_List;

   function Create_Parameter_List (Consumer_Key     : String;
                                   Signature_Method : String;
                                   Timestamp        : String;
                                   Nonce            : String;
                                   Version          : String := "")
     return Parameter_List.List is
      use Parameter_List;

      Params : List := Empty_List;
   begin
      Append (Params, (Key   => +"oauth_consumer_key",
                       Value => +Consumer_Key));
      Append (Params, (Key   => +"oauth_signature_method",
                       Value => +Signature_Method));
      Append (Params, (Key   => +"oauth_timestamp",
                       Value => +Timestamp));
      Append (Params, (Key   => +"oauth_nonce",
                       Value => +Nonce));
      if Version'Length > 0 then
         Append (Params, (Key   => +"oauth_version",
                          Value => +Version));
      end if;

      return Params;
   end Create_Parameter_List;

   function Create_Parameter_List (Consumer_Key     : String;
                                   Token            : String;
                                   Signature_Method : String;
                                   Timestamp        : String;
                                   Nonce            : String;
                                   Version          : String := "")
     return Parameter_List.List is
      use Parameter_List;

      Params : List := Empty_List;
   begin
      Append (Params, (Key   => +"oauth_consumer_key",
                       Value => +Consumer_Key));
      Append (Params, (Key   => +"oauth_token",
                       Value => +Token));
      Append (Params, (Key   => +"oauth_signature_method",
                       Value => +Signature_Method));
      Append (Params, (Key   => +"oauth_timestamp",
                       Value => +Timestamp));
      Append (Params, (Key   => +"oauth_nonce",
                       Value => +Nonce));
      if Version'Length > 0 then
         Append (Params, (Key   => +"oauth_version",
                          Value => +Version));
      end if;

      return Params;
   end Create_Parameter_List;

   function Create_Base_String (Method             : String;
                                URL                : String;
                                Request_Parameters : Parameter_List.List)
     return String is
      use Parameter_List;

      Params : List := Request_Parameters;
      Param_Str : Unbounded_String := Null_Unbounded_String;
      Pos : Cursor;
      Is_First : Boolean := True;
   begin
      Parameter_Sorting.Sort (Params);
      Pos := First (Params);
      loop
         exit when Pos = No_Element;
         if Is_First then
            Is_First := False;
         else
            Append (Param_Str, URL_Encode ("&"));
         end if;
         declare
            Pair_Str : Unbounded_String := Element (Pos).Key;
         begin
            Append (Pair_Str, URL_Encode ("="));
            Append (Pair_Str, URL_Encode (To_String (Element (Pos).Value)));
            Append (Param_Str, URL_Encode (To_String (Pair_Str)));
         end;
         Next (Pos);
      end loop;

      return Method & "&" & URL_Encode (URL) & "&" &
        To_String (Param_Str);
   end Create_Base_String;

   function Params_To_String (Request_Parameters : Parameter_List.List)
     return String is
      use Parameter_List;

      Params : List := Request_Parameters;
      Param_Str : Unbounded_String := Null_Unbounded_String;
      Pos : Cursor;
      Is_First : Boolean := True;
   begin
      Parameter_Sorting.Sort (Params);
      Pos := First (Params);
      loop
         exit when Pos = No_Element;
         if Is_First then
            Is_First := False;
         else
            Append (Param_Str, "&");
         end if;
         declare
            Pair_Str : Unbounded_String := Element (Pos).Key;
         begin
            Append (Pair_Str, "=");
            Append (Pair_Str, URL_Encode (To_String (Element (Pos).Value)));
            Append (Param_Str, To_String (Pair_Str));
         end;
         Next (Pos);
      end loop;

      return To_String (Param_Str);

   end Params_To_String;

   function Params_To_Header (Request_Parameters : Parameter_List.List)
     return String is
      use Parameter_List;

      Params : List := Request_Parameters;
      Param_Str : Unbounded_String := Null_Unbounded_String;
      Pos : Cursor;
      Is_First : Boolean := True;
   begin
      Append (Param_Str, "OAuth ");
      Pos := First (Params);
      loop
         exit when Pos = No_Element;
         if Is_First then
            Is_First := False;
         else
            Append (Param_Str, ", ");
         end if;
         declare
            Pair_Str : Unbounded_String := Element (Pos).Key;
         begin
            Append (Pair_Str, "=""");
            Append (Pair_Str,
              URL_Encode (To_String (Element (Pos).Value)) & '"');
            Append (Param_Str, To_String (Pair_Str));
         end;
         Next (Pos);
      end loop;

      return To_String (Param_Str);

   end Params_To_Header;

   function Nonce (Source_1 : String; Source_2 : String;
                   Source_3 : String := ""; Source_4 : String := "")
     return String is
      D : SHA.Digest;
   begin
      D := SHA.Process_Data.Digest_A_String
        (Source_1 & Source_2 & Source_3 & Source_4);
      return String (SHA.Strings.Hex_From_SHA (D));
   end Nonce;

   function Timestamp return String is
      use Ada.Strings.Fixed;
      use Ada.Strings;

      type Access_Int is access all Integer;
      function c_time (T : Access_Int) return Long_Integer;
      pragma Import (C, c_time, "time");

   begin
      return Trim (Long_Integer'Image (c_time (null)), Both);
   end Timestamp;
end OAuth;

