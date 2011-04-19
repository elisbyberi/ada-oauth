--  (C) Copyright 1999, John Halleck, All rights resurved.
--  Give a hash value for a string.
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;
with SHA.Process_Data; use SHA.Process_Data;
pragma Elaborate_All (SHA.Process_Data);

procedure SHA_Hex_String is

   Result : Hex_SHA_String;

begin

   if Argument_Count = 0 then
      Put_Line (Standard_Error, "??? Strings must be provided as arguments");
      Set_Exit_Status (Failure);
      return;
   end if;

   for I in 1 .. Argument_Count loop
      Result := Hex_From_SHA (Digest_A_String (Argument (I)));
      Put_Line (String (Result));
   end loop;

end SHA_Hex_String;
