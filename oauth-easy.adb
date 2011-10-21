with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with SHA;
with SHA.Strings;
with HMAC_SHA;
with HTTP;
with Hauki.Charbuf;

package body OAuth.Easy is
   use Hauki;
   use Hauki.Charbuf;
   
   function "+"(Str : String) return Unbounded_String
     renames To_Unbounded_String;

   function Get_Token (Context : OAuth_Context) return Unbounded_String is
   begin
      return Context.Token;
   end Get_Token;

   function Get_Token_Secret (Context : OAuth_Context)
     return Unbounded_String is
   begin
      return Context.Token_Secret;
   end Get_Token_Secret;

   procedure Set_Token (Context : in out OAuth_Context;
                        Token   :        String) is
   begin
      Context.Token := +Token;
   end Set_Token;

   procedure Set_Token_Secret (Context : in out OAuth_Context;
                               Secret  :        String) is
   begin
      Context.Token_Secret := +Secret;
   end Set_Token_Secret;

   procedure Init (Context         :    out OAuth_Context;
                   Consumer_Key    : in     String;
                   Consumer_Secret : in     String) is
   begin
      Context.Key := To_Unbounded_String (Consumer_Key);
      Context.Secret := To_Unbounded_String (Consumer_Secret);
      Context.Request_Count := 0;
   end Init;

   procedure Parse_Key_Value (Input : String;
                              Key : out Unbounded_String;
                              Value : out Unbounded_String) is
      Separator : Natural := Index (Source => Input, Pattern => "=");
   begin
      Separator := Index (Source => Input, Pattern => "=");
      if Separator = 0 then
         Key := Null_Unbounded_String;
         Value := Null_Unbounded_String;
         return;
      end if;

      Key := To_Unbounded_String
        (Input (Input'First .. Separator - 1));
      Value := To_Unbounded_String
        (Input (Separator + 1.. Input'Last));
   end Parse_Key_Value;

   procedure Parse_Request_Result (Input : String;
                                   Token : out Unbounded_String;
                                   Secret : out Unbounded_String) is
      Place : Natural := Input'First;
      Separator : Natural;
      Key, Value : Unbounded_String;
      Token_Found : Boolean := False;
      Secret_Found : Boolean := False;
   begin
      Token := Null_Unbounded_String;
      Secret := Null_Unbounded_String;

      loop
         exit when Token_Found and Secret_Found;
         Separator := Index
           (Source => Input (Place .. Input'Last),
            Pattern => "&");

         if Place = 0 and Separator = 0 then
            return;
         end if;

         if Separator > 0 then
            Parse_Key_Value (
              Input => Input (Place .. Separator - 1),
              Key   => Key,
              Value => Value);
         else
            Parse_Key_Value (
              Input => Input (Place .. Input'Last),
              Key   => Key,
              Value => Value);
         end if;

         if To_String (Key) = "oauth_token" then
            Token := Value;
            Token_Found := True;
         elsif To_String (Key) = "oauth_token_secret" then
            Secret := Value;
            Secret_Found := True;
         end if;

         Place := Separator + 1;
         exit when Place >= Input'Last or Separator = 0;
      end loop;
   end Parse_Request_Result;

   procedure Parse_Access_Token (Input : String;
                                 Token : out Unbounded_String;
                                 Secret : out Unbounded_String;
                                 User_Id : out Unbounded_String;
                                 Name : out Unbounded_String) is
      Place : Natural := Input'First;
      Separator : Natural;
      Key, Value : Unbounded_String;
      Token_Found : Boolean := False;
      Secret_Found : Boolean := False;
      User_Id_Found : Boolean := False;
      Name_Found : Boolean := False;
   begin
      Token := Null_Unbounded_String;
      Secret := Null_Unbounded_String;
      User_Id := Null_Unbounded_String;
      Name := Null_Unbounded_String;

      loop
         exit when Token_Found and Secret_Found and
                   User_Id_Found and Name_Found;
         Separator := Index
           (Source => Input (Place .. Input'Last),
            Pattern => "&");

         if Place = 0 and Separator = 0 then
            return;
         end if;

         if Separator > 0 then
            Parse_Key_Value (
              Input => Input (Place .. Separator - 1),
              Key   => Key,
              Value => Value);
         else
            Parse_Key_Value (
              Input => Input (Place .. Input'Last),
              Key   => Key,
              Value => Value);
         end if;

         if To_String (Key) = "oauth_token" then
            Token := Value;
            Token_Found := True;
         elsif To_String (Key) = "oauth_token_secret" then
            Secret := Value;
            Secret_Found := True;
         elsif To_String (Key) = "user_id" then
            User_Id := Value;
            User_Id_Found := True;
         elsif To_String (Key) = "screen_name" then
            Name := Value;
            Name_Found := True;
         end if;

         Place := Separator + 1;
         exit when Place >= Input'Last or Separator = 0;
      end loop;
   end Parse_Access_Token;

   procedure Request_Token (Context      : in out OAuth_Context;
                            URL          : in     String;
                            Method       : in     String) is
      Params : OAuth.Parameter_List.List;
      Digest_Val : SHA.Digest;
      Base_Str : Unbounded_String;
      Ctx : HMAC_SHA.HMAC_Context;
      Sig : Unbounded_String;
      H : Unbounded_String;
      Result : Charbuf.Char_Buffer;
      R_Code : Long_Integer;
   begin
      Params := OAuth.Create_Parameter_List
        (Consumer_Key => To_String (Context.Key),
         Signature_Method => "HMAC-SHA1",
         Timestamp    => OAuth.Timestamp,
         Nonce        => OAuth.Nonce
           (Natural'Image (Context.Request_Count),
            OAuth.Timestamp,
            To_String (Context.Key) & URL),
         Version      => "1.0");
      OAuth.Parameter_List.Append (Params, (Key => +"oauth_callback",
                                            Value => +"oob"));

      Base_Str := To_Unbounded_String (OAuth.Create_Base_String
        (Method, URL, Params));
      Put_Line ("Base string: " & To_String (Base_Str));

      HMAC_SHA.Initialize (Key => To_String (Context.Secret) & "&",
                           Context => Ctx);
      HMAC_SHA.Add (Message => To_String (Base_Str),
                    Context => Ctx);
      HMAC_SHA.Finalize (Result => Digest_Val,
                         Context => Ctx);
      Sig := +(String (SHA.Strings.B64_From_SHA (Digest_Val)));

      OAuth.Parameter_List.Append (Params, (Key => +"oauth_signature",
                                            Value => Sig));
      OAuth.Parameter_Sorting.Sort (Params);
      OAuth.Parameter_List.Prepend (Params, (Key => +"realm",
                                            Value => +""));
      H := To_Unbounded_String (OAuth.Params_To_Header (Params));
      Put_Line ("Header: " & To_String (H));
      HTTP.Get_Page
        (URL => URL,
         Header_Key => "Authorization",
         Header_Value => To_String (H),
         Contents => Result,
         Response_Code => R_Code);
      Put_Line ("Result: " & To_String (Result));
      Parse_Request_Result
        (To_String (Result), Context.Token, Context.Token_Secret);
   end Request_Token;

   procedure Access_Token (Context      : in out OAuth_Context;
                           URL          : in     String;
                           Method       : in     String;
                           Verifier     : in     String;
                           User_Id      :    out Unbounded_String;
                           Name         :    out Unbounded_String) is

      Temp_Token : String := To_String (Context.Token);
      Temp_Secret : String := To_String (Context.Token_Secret);
      Params : OAuth.Parameter_List.List;
      Digest_Val : SHA.Digest;
      Base_Str : Unbounded_String;
      Ctx : HMAC_SHA.HMAC_Context;
      Sig : Unbounded_String;
      H : Unbounded_String;
      Result : Charbuf.Char_Buffer;
      R_Code : Long_Integer;
   begin
      Params := OAuth.Create_Parameter_List
        (Consumer_Key => To_String (Context.Key),
         Token        => Temp_Token,
         Signature_Method => "HMAC-SHA1",
         Timestamp    => OAuth.Timestamp,
         Nonce        => OAuth.Nonce
           (Natural'Image (Context.Request_Count),
            OAuth.Timestamp,
            To_String (Context.Key) & URL),
         Version      => "1.0");
      OAuth.Parameter_List.Append (Params, (Key => +"oauth_verifier",
                                            Value => +Verifier));
      OAuth.Parameter_Sorting.Sort (Params);
      Base_Str := +(OAuth.Create_Base_String (Method, URL, Params));
      Put_Line ("Base string: " & To_String (Base_Str));

      HMAC_SHA.Initialize
        (Key     => To_String (Context.Secret) & "&" & Temp_Secret,
         Context => Ctx);
      HMAC_SHA.Add (Message => To_String (Base_Str),
                    Context => Ctx);
      HMAC_SHA.Finalize (Result => Digest_Val,
                         Context => Ctx);
      Sig := +(String (SHA.Strings.B64_From_SHA (Digest_Val)));
      OAuth.Parameter_List.Append (Params, (Key => +"oauth_signature",
                                            Value => Sig));
      OAuth.Parameter_Sorting.Sort (Params);
      OAuth.Parameter_List.Prepend (Params, (Key => +"realm",
                                            Value => +""));
      H := +(OAuth.Params_To_Header (Params));
      Put_Line ("Header: " & To_String (H));
      HTTP.Get_Page
        (URL => URL,
         Header_Key => "Authorization",
         Header_Value => To_String (H),
         Contents => Result,
         Response_Code => R_Code);
      Put_Line ("Result: " & To_String (Result));
      Parse_Access_Token
        (To_String (Result), Context.Token, Context.Token_Secret,
         User_Id, Name);
   end Access_Token;

   procedure As_Header (Context    : in out OAuth_Context;
                        URL        : in     String;
                        Method     : in     String;
                        Parameters : in     OAuth.Parameter_List.List;
                        Header     :    out Unbounded_String) is
      Temp_Token : constant String := To_String (Context.Token);
      Temp_Secret : constant String := To_String (Context.Token_Secret);
      Params : OAuth.Parameter_List.List := Parameters;
      OAuth_Params : OAuth.Parameter_List.List := Parameters;
      Digest_Val : SHA.Digest;
      Base_Str : Unbounded_String;
      Ctx : HMAC_SHA.HMAC_Context;
      Sig : Unbounded_String;
      Result : Unbounded_String;
   begin
      OAuth_Params := OAuth.Create_Parameter_List
        (Consumer_Key => To_String (Context.Key),
         Token        => Temp_Token,
         Signature_Method => "HMAC-SHA1",
         Timestamp    => OAuth.Timestamp,
         Nonce        => OAuth.Nonce
           (Natural'Image (Context.Request_Count),
            OAuth.Timestamp,
            To_String (Context.Key) & URL),
         Version      => "1.0");
      OAuth.Parameter_Sorting.Sort (OAuth_Params);
      OAuth.Parameter_Sorting.Sort (Params);
      OAuth.Parameter_Sorting.Merge (Target => Params, Source => OAuth_Params);
      Base_Str := +(OAuth.Create_Base_String (Method, URL, Params));
      Put_Line ("Base string: " & To_String (Base_Str));
      HMAC_SHA.Initialize
        (Key     => To_String (Context.Secret) & "&" & Temp_Secret,
         Context => Ctx);
      HMAC_SHA.Add (Message => To_String (Base_Str),
                    Context => Ctx);
      HMAC_SHA.Finalize (Result => Digest_Val,
                         Context => Ctx);
      Sig := +(String (SHA.Strings.B64_From_SHA (Digest_Val)));
      OAuth.Parameter_List.Append (Params, (Key => +"oauth_signature",
                                            Value => Sig));
      OAuth.Parameter_List.Prepend (Params, (Key => +"realm",
                                            Value => +""));
      OAuth.Parameter_Sorting.Sort (Params);
      Header := +(OAuth.Params_To_Header (Params));
   end As_Header;

end OAuth.Easy;
