--  (C) Copyright 1999, John Halleck, All rights resurved.
--  Give a Base 64 encoded hash value for a string.
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;
with SHA.Process_Data; use SHA.Process_Data;
pragma Elaborate_All (SHA.Process_Data);

procedure SHA_B64_String is

   Result : B64_SHA_String;

begin

   if Argument_Count = 0 then
      Put_Line (Standard_Error, "??? Strings must be provided as arguments");
      Set_Exit_Status (Failure);
      return;
   end if;

   for I in 1 .. Argument_Count loop
      Result := B64_From_SHA (Digest_A_String (Argument (I)));
      Put_Line (String (Result));
   end loop;

end SHA_B64_String;
