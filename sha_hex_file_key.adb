--  (C) Copyright 1999, John Halleck, All rights resurved.
--  Give a Hex hash value for a file.
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;

with Interfaces;
with Utils;
with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;
with SHA.Process_Data; use SHA.Process_Data;
pragma Elaborate_All (Sha.Process_Data);

procedure SHA_Hex_File_Key is
   package USA renames Ada.Strings.Unbounded;
   use type Interfaces.Unsigned_32;

   Given : File_Type;

   Result : Hex_SHA_String;

   Input : Character;

   Ipad : constant := 16#36363636#;
   Opad : constant := 16#5C5C5C5C#;

   Key_Str : USA.Unbounded_String := USA.Null_Unbounded_String;
   type U32_Array is array(1..16) of Interfaces.Unsigned_32;
   Key : U32_Array;
   Key_Digest : Digest;
   Ipad_Key : U32_Array;
   Opad_Key : U32_Array;
   Msg : USA.Unbounded_String := USA.Null_Unbounded_String;
   K_Index : Positive;

begin
   if Argument_Count < 2 then
      Put_Line ("??? key and file names must be provided as arguments");
      Set_Exit_Status (Failure);
      return;
   end if;
   Key_Str := USA.To_Unbounded_String (Argument (1));
   if USA.Length (Key_Str) > 64 then
      Key_Digest := Digest_A_String (Argument (1));
      Key := (others => 0);
      K_Index := Key'First;
      for I in Key_Digest'Range loop
         Key (K_Index) := Key_Digest (I);
         K_Index := K_Index + 1;
      end loop;
   else
      declare
         Str : constant String := Argument (1);
         Place : Natural := Str'First;
         Key_Index : Natural := 1;
      begin
         loop
            exit when Place > Str'Last or Key_Index > Key'Last;
            if Str'Last - Place >= 3 then
               Key (Key_Index) :=
                 Utils.String_to_U32 (Str (Place .. Place + 3));
            else
               Key (Key_Index) :=
                 Utils.String_to_U32 (Str (Place .. Str'Last));
            end if;

            Place := Place + 4;
            Key_Index := Key_Index + 1;
         end loop;
         loop
            exit when Key_Index > Key'Last;
            Key (Key_Index) := 0;
            Key_Index := Key_Index + 1;
         end loop;
      end;
   end if;

   for I in Key'Range loop
      Ipad_Key (I) := Ipad xor Key(I);
      Opad_Key (I) := Opad xor Key(I);
   end loop;
   for I in Ipad_Key'Range loop
      Put (Utils.U32_to_Hex (Ipad_Key(I)));
   end loop;
   New_Line;
   for I in Opad_Key'Range loop
      Put (Utils.U32_to_Hex (Opad_Key(I)));
   end loop;
   New_Line;

   for I in 2 .. Argument_Count loop
      begin

         --  Unfortunately, Ada does not have "raw" file IO defined for
         --  files, so the kludge below is used to read each 8 bit
         --  character from the file.
         --  On machines like the Sperry Univac 1100 series machines
         --  bytes are four nine-bit bytes per word, and this doesn't
         --  work at all.   Since the SHA hash is defined based on bits
         --  (and only by extension for bytes) any file on any machine should
         --  be hashable.   The limitations of the standard IO package in Ada
         --  don't allow a machine indepentdent way of doing this.
         --  SO...  the hack below works on PC's, Macintoshes, and Unix boxes,
         --  (Where a file may be completely read as 8 bit bytes.) But it
         --  is far from being portable.

         Open  (Given, In_file, Argument (I));
         Initialize;

         loop
            begin
               Get_Immediate (Given, Input);
               USA.Append (Msg, Input);
               Add (Byte (Character'Pos (Input)));
            exception
               when End_Error =>
                  exit;
            end;
         end loop;
         Close (Given);
         Result := Hex_From_SHA (Finalize);
         Put_Line (String (Result));

         declare
            I_Ctx : Context;
            O_Ctx : Context;
            I_Result : Digest;
            O_Result : Digest;
         begin
            Put ("ipaddata: ");
            Initialize (I_Ctx);
            for I in Ipad_Key'Range loop
               Put (Utils.U32_To_Hex (Ipad_Key (I)));
               Add (Long (Ipad_Key (I)), I_Ctx);
            end loop;
            for I in 1 .. USA.Length (Msg) loop
               declare
                  Char : constant Character := USA.Element (Msg, I);
               begin
                  Put (Utils.Byte_To_Hex (Interfaces.Unsigned_8 (Character'Pos (Char))));
                  Add (Byte (Character'Pos (Char)), I_Ctx);
               end;
            end loop;
            New_Line;
            Finalize (I_Result, I_Ctx);
            Put ("ipad: ");
            for I in I_Result'Range loop
               Put (Utils.U32_to_Hex (I_Result(I)));
            end loop;
            New_Line;
            Put_Line ("ipad: " & String (Hex_From_SHA (I_Result)));

            Initialize (O_Ctx);
            for I in Opad_Key'Range loop
               Add (Long (Opad_Key (I)), O_Ctx);
            end loop;
            for I in I_Result'Range loop
               -- Put (Utils.U32_to_Hex (I_Result (I)));
               Add (Long (I_Result (I)), O_Ctx);
            end loop;
            Finalize (O_Result, O_Ctx);
            Result := Hex_From_SHA (O_Result);
            Put ("opad: ");
            Put_Line (String (Result));
         end;

      exception
         when Name_Error =>
            Put (Standard_Error, "File """);
            Put (Standard_Error, Argument (I));
            Put_Line (Standard_Error, """ was a bad name.");
            Set_Exit_Status (Failure);
         when Status_Error =>
            Put (Standard_Error, Argument (I));
            Put_Line (Standard_Error, " Doesn't seem to exist.");
            Set_Exit_Status (Failure);
      end;
   end loop;

end SHA_Hex_File_Key;
