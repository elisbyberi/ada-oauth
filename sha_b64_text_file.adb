--  (C) Copyright 1999, John Halleck,  All rights resurved.
--  Give a Base 64 hash value for a Text file.
--  This requires a cannonicalization of the system EOL sequence to the network
--  standard <CR> <LF>

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;
with SHA.Process_Data; use SHA.Process_Data;
pragma Elaborate_All (Sha.Process_Data);

procedure SHA_B64_Text_File is

   Given : File_Type;

   Result : B64_SHA_String;

   Input : Character;

begin

   if Argument_Count = 0 then
      Put_Line (Standard_Error,
                "??? file names must be provided as arguments");
      Set_Exit_Status (Failure);
      return;
   end if;

   for I in 1 .. Argument_Count loop
      begin

         Open  (Given, In_file, Argument (I));
         Initialize;

         loop
            begin
               if End_Of_File (Given) then
                  Add (Byte (8#15#)); -- <CR>
                  Add (Byte (8#12#)); -- <LF>
                  exit;
               elsif End_Of_Page (Given) then
                  Add (Byte (8#15#)); -- <CR>
                  Add (Byte (8#12#)); -- <LF>
                  Add (Byte (8#14#)); -- <FF>
                  Skip_Page (Given);
               elsif End_Of_Line (Given) then
                  Add (Byte (8#15#)); -- <CR>
                  Add (Byte (8#12#)); -- <LF>
                  Skip_Line (Given);
               else
                  Put_Line ("Debug: doing character...");
                  Get_Immediate (Given, Input);
                  Add (Byte (Character'Pos (Input)));
               end if;
            exception
               when End_Error =>
                  exit;
            end;
         end loop;
         Close (Given);
         Result := B64_From_SHA (Finalize);
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

end SHA_B64_Text_File;
