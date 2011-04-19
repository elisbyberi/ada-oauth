--  (C) Copyright 1999, John Halleck, All rights resurved.
--  Run a set of NIST tests from one of their files, producing an
--  appropriate output file to send back..
--  This is part of a project documented at http://www.cc.utah.edu/~nahaj/

with Ada.Sequential_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

with SHA;                 use SHA;
with SHA.Strings;         use SHA.Strings;
with SHA.Process_Data;    use SHA.Process_Data;

procedure SHA_Old_NIST_Tests is

   type Status is (Scanning_for_Test,
                   Scanning_for_Bits,
                   Scanning_number);

   Done : Boolean := False;
   Given, Producing : File_Type;

   Result : Hex_SHA_String;

   Input : Integer;
   --  Test  : Character;

   Z, B : Integer;  -- From the NIST vector Readme file.
   Length : Integer := 0;
   Count  : Integer := 0;

begin

   Set_Exit_Status (Success);

   if Argument_Count > 0 then
      Put_Line ("??? This program doesn't support arguments.");
      Set_Exit_Status (Failure);
      return;
   end if;

   while not Done loop

      begin
         Get (Z);
         Get (B);
         Count := Count + 1;

         Length := 0;
         Initialize;
         begin
            for J in 1 .. Z loop
               Get (Input);
               Length := Length + Input;

               for I in 1 .. Input loop
                  Add (Bit (B));
               end loop;

               B := 1 - B;
            end loop;
            Skip_Line;

         exception
            when End_Error | Data_Error =>
               Done := True;
               exit;
         end;

         Result := Hex_From_SHA (Finalize);
         --  Put ("Length of bit string "); Put (Count);
         --  Put (" := "); Put (Length); New_Line;
         Put (String (Result));
         Put_Line (" ^");
   

      exception
         when End_Error | Data_Error =>
            Done := True;
            exit;
      end;

   end loop;


exception

   when others =>
      Set_Exit_Status (Failure);
      raise;

end SHA_Old_NIST_Tests;
