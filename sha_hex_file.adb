--  (C) Copyright 1999, John Halleck, All rights resurved.
--  Give a Hex hash value for a file.
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;
with SHA.Process_Data; use SHA.Process_Data;
pragma Elaborate_All (Sha.Process_Data);

procedure SHA_Hex_File is

   Given : File_Type;

   Result : Hex_SHA_String;

   Input : Character;

begin

   if Argument_Count = 0 then
      Put_Line ("??? file names must be provided as arguments");
      Set_Exit_Status (Failure);
      return;
   end if;

   for I in 1 .. Argument_Count loop
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
               Add (Byte (Character'Pos (Input)));
            exception
               when End_Error =>
                  exit;
            end;
         end loop;
         Close (Given);
         Result := Hex_From_SHA (Finalize);
         Put_Line (String (Result));

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

end SHA_Hex_File;
