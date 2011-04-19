--  (C) Copyright 2000, John Halleck, All rights resurved.
--  Run a set of NIST tests from one of their files, producing an
--  appropriate output file to send back..
--  This is part of a project documented at http://www.cc.utah.edu/~nahaj/

--  This program read's a NIST request file, and produces the responce file
--  per the documentation located at http://csrc.ncsl.nist.gov/cryptval/
--  The call of this program is of the form:
--     SHA_NIST_Tests  input.req  output.rsp

--  We'll need to read and write...
with Ada.Text_IO;         use Ada.Text_IO;

--  And deal with any command line arguments.
with Ada.Command_Line;    use Ada.Command_Line;

--  We are testing SHA so we need it.
with SHA;                 use SHA;
with SHA.Strings;         use SHA.Strings;
with SHA.Process_Data;    use SHA.Process_Data;
pragma Elaborate_All (Sha.Process_Data);

--  We will need the routines specific to our use of NIST tests.
with NIST_Support;        use NIST_Support;

procedure SHA_NIST_Tests is

   Request, Response : File_Type;

   Result_Digest : Digest;
   Result : Hex_SHA_String;

begin

   Set_Exit_Status (Success);
   Put_Line (Standard_Output, "SHA-1 tests from NIST test file");

   if Argument_Count /= 2 then
      Put_Line (Standard_Error, "****  Wrong number of Arguments!");
      Put_Line (Standard_Error, "Call should be:");
      Put_Line (Standard_Error, "   sha_nist_tests inputfile outputfile");
   end if;

   if Argument_Count = 0 then
      Put_Line (Standard_Error, "*** No input and output files given");
      Set_Exit_Status (Failure);
      return;
   elsif Argument_Count = 1 then
      Put_Line (Standard_Error, "*** No output file given");
      Set_Exit_Status (Failure);
      return;
   elsif Argument_Count > 2 then
      Put_Line (Standard_Error, "*** Too many arguments given.");
      Set_Exit_Status (Failure);
      return;
   end if;

   Put (Standard_Output, "   Input file : ");
   Put_Line (Standard_Output, Argument (1));
   begin
      Open (Request, In_File, Argument (1));
   exception
      when Name_Error =>
         Set_Exit_Status (Failure);
         Put (Standard_Error, "*** No such input file: ");
         Put_Line (Standard_Error, Argument (1));
         return;
   end;

   Put (Standard_Output, "   Output file: ");
   Put_Line (Standard_Output, Argument (2));
   begin
      Create (Response, Out_File, Argument (2));
   exception
      when Name_Error =>
         Set_Exit_Status (Failure);
         Put (Standard_Error, "*** File already exists: ");
         Put_Line (Standard_Error, Argument (2));
         return;
   end;

   Put_Line (Standard_Output, "   (Opened files...)");

   --  The documentation from NIST has the SHA-1 response file as the ONLY
   --  responce type that doesn't require the request file comments echoed
   --  to the output.   HOWEVER, the examples all have them, and they should
   --  be there.  So...
   Put_Line (Standard_Output, "   (Copying comments...)");
   Copy_Comments (Request, Response);

   Put_Line (Standard_Output, "   (Starting tests...)");

   --  Type one tests. -----------------------------------------------

   Get_String (Request, "H>SHS Type 1 Strings<H", "First test header");
   Put_Line (Response, "H>SHS Type 1 Hashes<H");

   Get_String (Request, "D>", "First test, Data Start");
   Put_Line (Response, "D>");
   Put_Line (Standard_Output, "   Starting SHA-1 type 1 tests...");

   while Find_Next_Bit_String (Request) loop
      Put (Response, String (Hex_From_SHA
          (Hash_Bit_String (Request))));
      Put_Line (Response, " ^");
   end loop;

   Get_String (Request, "<D", "First test, data end");
   Put_Line (Response, "<D"); New_Line (Response);
   Put_Line (Standard_Output, "   Finished SHA-1 type 1 tests.");

   --  Type Two tests. --------------------------------------------------------

   Get_String (Request, "H>SHS Type 2 Strings<H", "Second test header");
   Put_Line (Response, "H>SHS Type 2 Hashes<H");

   Get_String (Request, "D>", "Second test Data");
   Put_Line (Response, "D>");
   Put_Line (Standard_Output, "   Starting SHA-1 type 2 tests...");

   while Find_Next_Bit_String (Request) loop
      Result := Hex_From_SHA (Hash_Bit_String (Request));
      Put (Response, String (Result)); Put_Line (Response, " ^");
   end loop;

   Get_String (Request, "<D", "Second test, data end");
   Put_Line (Response, "<D"); New_Line (Response);
   Put_Line (Standard_Output, "   Finished SHA-1 type 2 tests.");

   --  Type Three Tests. -----------------------------------------------------

   Get_String (Request, "H>SHS Type 3 Strings<H", "Third test header");
   Put_Line (Response, "H>SHS Type 3 Hashes<H");

   Get_String (Request, "D>", "Third test, data start");
   Put_Line (Response, "D>");
   Put_Line (Standard_Output, "   Starting SHA-1 type 3 tests.");
   Put_Line (Standard_Output, "      (This may take a LONG time ...)");

   while Find_Next_Bit_String (Request) loop
      Result_Digest := (others => 0);
      Initialize;
      Add_Bit_String_To_Hash (Request);
      for J in 0 .. 99 loop
         if J = 25 + 1 then
            Put_Line (Standard_Output, "      (Test 1/4 finished)");
         elsif J = 50 + 1 then
            Put_Line (Standard_Output, "      (Test 1/2 finished)");
         elsif J = 75 + 1 then
            Put_Line (Standard_Output, "      (Text 3/4 finished)");
         end if;
         for I in 1 .. Long (50_000) loop
            for A in 1 .. (J / 4) * 8 + 24 loop
               Add (Bit (0));
            end loop;
            Add (I);
            Finalize (Result_Digest);
            Initialize;
            for K in Digest'First .. Digest'Last loop
               Add (Long (Result_Digest (K)));
            end loop;
         end loop;
         Put (Response, String (Hex_From_SHA (Result_Digest)));
         Put_Line (Response, " ^");
      end loop;
      Finalize (Result_Digest);
   end loop;

   Get_String (Request, "<D", "Third test, data end");
   Put_Line (Response, "<D"); New_Line (Response);
   Put_Line (Standard_Output, "   Finished SHA-1 type 3 tests.");

   --  Done. -----------------------------------------------------------------

   Put_Line (Standard_Output, "   (Tests finished.  Closing files...)");

   Close (Request);
   Close (Response);

   Put_Line (Standard_Output, "SHA-1 NIST tests done.");

exception

   when others =>
      Set_Exit_Status (Failure);
      raise;

end SHA_NIST_Tests;
