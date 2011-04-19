--  (C) Copyright 2000, John Halleck, All rights resurved.
--  Given a correct NIST request and response file, verify that the visible
--  SHA-1 implementation given that request would produce that result.
--  This is part of a project documented at http://www.cc.utah.edu/~nahaj/
--  The files are documentated at http://csrc.ncsl.nist.gov/cryptval/

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

procedure SHA_NIST_Verify is

   Request, Response : File_Type;

   Result_Digest : Digest;

begin

   Set_Exit_Status (Success);
   Put_Line (Standard_Output, "SHA-1 verification from NIST test files");

   if Argument_Count /= 2 then
      Put_Line (Standard_Error, "****  Wrong number of Arguments!");
      Put_Line (Standard_Error, "Call should be:");
      Put_Line (Standard_Error, "   sha_nist_tests requestfile responsefile");
   end if;

   if Argument_Count = 0 then
      Put_Line (Standard_Error, "*** No request and response files given");
      Set_Exit_Status (Failure);
      return;
   elsif Argument_Count = 1 then
      Put_Line (Standard_Error, "*** No response file to compare with given");
      Set_Exit_Status (Failure);
      return;
   elsif Argument_Count > 2 then
      Put_Line (Standard_Error, "*** Too many arguments given.");
      Set_Exit_Status (Failure);
      return;
   end if;

   Put (Standard_Output, "   Request file : ");
   Put_Line (Standard_Output, Argument (1));
   begin
      Open (Request, In_File, Argument (1));
   exception
      when Name_Error =>
         Set_Exit_Status (Failure);
         Put (Standard_Error, "*** No such Request file: ");
         Put_Line (Standard_Error, Argument (1));
         return;
   end;

   Put (Standard_Output, "   Response file: ");
   Put_Line (Standard_Output, Argument (2));
   begin
      Open (Response, In_File, Argument (2));
   exception
      when Name_Error =>
         Set_Exit_Status (Failure);
         Put (Standard_Error, "*** No such response file: ");
         Put_Line (Standard_Error, Argument (2));
         return;
   end;

   Put_Line (Standard_Output, "   (Opened files...)");

   Put_Line (Standard_Output, "   (Starting tests...)");

   --  Type one tests. -----------------------------------------------

   Get_String (Request,  "H>SHS Type 1 Strings<H", "First test header");
   Get_String (Response, "H>SHS Type 1 Hashes<H", "First response header");
   Get_String (Request, "D>", "First test, Data Start");
   Get_String (Response, "D>", "First test, response start");
   Put_Line (Standard_Output, "   Starting SHA-1 type 1 tests.");

   while Find_Next_Bit_String (Request) loop
      if not Find_Hash (Response) then
         Put_Line (Standard_Error, "Couldn't find Hash in response file?");
         raise NIST_Test_Failed_Error;
      end if;
      Compare_Hash (Response, Hex_From_SHA (Hash_Bit_String (Request)));
   end loop;

   Get_String (Request, "<D", "First test, data end");
   Put_Line (Standard_Output, "   Finished SHA-1 type 1 tests.");

   --  Type Two tests. --------------------------------------------------------

   Get_String (Request, "H>SHS Type 2 Strings<H", "Second test header");
   Get_String (Response, "H>SHS Type 2 Hashes<H", "Second response header");
   Get_String (Request, "D>", "Second test Data");
   Get_String (Response, "D>", "Start of second set of hashes.");
   Put_Line (Standard_Output, "   Starting SHA-1 type 2 tests.");

   while Find_Next_Bit_String (Request) loop
      if not Find_Hash (Response) then
         raise NIST_Test_Failed_Error;
      end if;
      Compare_Hash (Response, Hex_From_SHA (Hash_Bit_String (Request)));
   end loop;

   Get_String (Request, "<D", "Second test, data end");
   Put_Line (Standard_Output, "   Finished SHA-1 type 2 tests.");

   --  Type Three Tests. -----------------------------------------------------

   Get_String (Request, "H>SHS Type 3 Strings<H", "Third test header");
   Get_String (Response, "H>SHS Type 3 Hashes<H", "Third hash header");
   Get_String (Request, "D>", "Third test, data start");
   Get_String (Response, "D>", "Third test, hash start");
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
         if not Find_Hash (Response) then
            raise NIST_Test_Failed_Error;
         end if;
         Compare_Hash (Response, Hex_From_SHA (Result_Digest));
      end loop;
      Finalize (Result_Digest);
   end loop;

   Get_String (Request, "<D", "Third test, data end");
   Put_Line (Standard_Output, "   Finished SHA-1 type 3 tests.");

   --  Done. -----------------------------------------------------------------

   Put_Line (Standard_Output, "   (Tests finished.  Closing files...)");

   Close (Request);
   Close (Response);

   Put_Line (Standard_Output, "SHA-1 NIST tests PASSED.");

exception

   when others =>
      Put_Line (Standard_Error, "SHA-1 NIST testing FAILED.");
      Set_Exit_Status (Failure);
      raise;

end SHA_NIST_Verify;
