--  (C) Copyright 1999, John Halleck,  All Rights Reserved
--  Test the SHA package
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

with Ada.Text_IO;      use Ada.Text_IO;

with Interfaces;       use Interfaces;  --  We need bit rotates...

with Ada.Command_Line; use Ada.Command_Line;

with SHA;              use SHA;              -- Definition of a digest.
with SHA.Strings;      use SHA.Strings;      -- Convert Digest to String.
with SHA.Process_Data; use SHA.Process_Data; -- Compute Digests.
pragma Elaborate_All (SHA.Process_Data);


procedure SHA_Process_Data_TEST is

   Expected : Digest :=
       (16#A9993E36#, 16#4706816A#, 16#BA3E2571#, 16#7850C26C#, 16#9CD0D89D#);

   Expected_Second_Test : constant Digest :=
       (16#84983E44#, 16#1C3BD26E#, 16#BAAE4AA1#, 16#F95129E5#, 16#E54670F1#);
   Expected_Third_Test  : constant Digest :=
       (16#34AA973C#, 16#D4C4DAA4#, 16#F61EEB2B#, 16#DBAD2731#, 16#6534016F#);

   Computed : Digest;
   Actual   : Digest;


   type Test_Index is new Natural range 0 .. ((256 * 8) - 1);
   Buffer : array (Test_Index) of Boolean;


   Result    : Exit_Status := Success;

   procedure Verify (Assumed, Given : Digest);
   --  (Keep over agressive style checkers happy.)
   procedure Verify (Assumed, Given : Digest) is
   begin
      if Assumed = Given then
         Put_Line (" Passed!");
      else
         Put_Line (" *** FAILED ***");
         Put ("     Expected: ");
         Put_Line (String (Hex_From_SHA (Assumed)));
         Put ("          Got: ");
         Put_Line (String (Hex_From_SHA (Given)));
         Result := Failure;
      end if;
   end Verify;

   function Get_Byte (Index : Test_Index) return Byte;
   function Get_Byte (Index : Test_Index) return Byte is
      Result : Byte := 0;
   begin
      for I in 0 .. Test_Index (8 - 1) loop
         Result := Result * 2;
         if Buffer (I + Index) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Get_Byte;

   function Get_Word (Index : Test_Index) return Word;
   function Get_Word (Index : Test_Index) return Word is
      Result : Word := 0;
   begin
      for I in 0 .. Test_Index (16 - 1) loop
         Result := Result * 2;
         if Buffer (I + Index) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Get_Word;

   function Get_Long (Index : Test_Index) return Long;
   function Get_Long (Index : Test_Index) return Long is
      Result : Long := 0;
   begin
      for I in 0 .. Test_Index (32 - 1) loop
         Result := Result * 2;
         if Buffer (I + Index) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Get_Long;

   function Get_Integer (Index : Test_Index; Size : Positive)
      return Unsigned_32;
   function Get_Integer (Index : Test_Index; Size : Positive)
      return   Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      for I in Index .. Index + Test_Index (Size - 1) loop
         Result := Result * 2;
         if Buffer (I + Index) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Get_Integer;

   procedure Put_Byte (Index : Test_Index; Given : Unsigned_8);
   procedure Put_Byte (Index : Test_Index; Given : Unsigned_8) is
   begin
      for I in 0 .. 7 loop
         Buffer (Test_Index (I) + Index) := (Given and 2 ** (7 - I)) /= 0;
      end loop;
   end Put_Byte;

   procedure Set_Bit (Which : Boolean);
   procedure Set_Bit (Which : Boolean) is
   begin
      if Which then Add (Bit (1)); else Add (Bit (0)); end if;
   end Set_Bit;

begin

   Put_Line ("SHA.Process_Data test [Secure Hash Algorithm] starting ...");


   Put_Line ("   Testing SHA from FIPS 180-1 first test");

   Actual := Digest_A_String ("abc");
   Verify (Expected, Actual);

   --   But that doesn't test the padding and flushing code...

   Put ("   Testing SHA-1 second test from standard ...");

   Actual := Digest_A_String
            ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
   Verify (Expected_Second_Test, Actual);

   --  We've now tested a string that forced us to go over the block boundary.
   --  So, it is probably right...

   --  --  --  Third test from SHA-1 standard tests.

   Put ("   Testing SHA-1 million ""a""'s test (takes a while) ...");

   Initialize;
   for I in 1 .. 1000000 loop
      Add (Byte'(Character'Pos ('a')));
   end loop;
   Finalize (Actual);
   Verify (Expected_Third_Test, Actual);

   --  This test forced use of a count larger than 32K, and exercised most
   --  things very completely.  It is almost impossible to imagine something
   --  that could pass all three tests and not be correct.

   --  -- -------------------  Individual bit tests. --------------------------
   --  Since my package has the ability to hash arbitrary bit strings, and the
   --  tests above are only byte oriented, there is code that has not been
   --  tested.   We will now test using the primatives that add individual
   --  bits so that we can force tests of all alignments.

   Put ("   ===  Support routines alignment checks:");

   --  What should the answer be?
   Initialize;
   for I in 0 .. 255 loop
      Add (Byte (I));
   end loop;
   Finalize (Actual);
   --  We've just tested the byte versions, so we will use them as a reference.

   --  Ok, Fill in the bits.
   for I in 0 .. 255 loop
      Put_Byte (Test_Index (I * 8), Unsigned_8 (I));
   end loop;
   Put_Line (" (Reference initialized)");

   --  And what do we get with just adding bits?
   Initialize;
   Put ("   Testing hash of single bits...");
   for I in Test_Index loop
      Set_Bit (Buffer (I));
   end loop;
   Finalize (Computed);
   Verify (Actual, Computed);

--  If this worked, we know that the main bit add on works... at lease somewhat

   Put ("   Testing hash of generated bytes...");
   Initialize;
   for I in Test_Index'First .. Test_Index'Last / 8 loop
      Add (Get_Byte (I * 8));
   end loop;
   Finalize (Computed);
   Verify (Actual, Computed);
--  We know know that the byte primitives in this file work.
   Put ("   Testing hash of generated words...");
   Initialize;
   for I in Test_Index'First .. Test_Index'Last / 16 loop
      Add (Get_Word (I * 16));
   end loop;
   Finalize (Computed);
   Verify (Actual, Computed);
--  We know that the word primitives work.
   Put ("   Testing hash of generated Long...");
   Initialize;
   for I in Test_Index'First .. Test_Index'Last / 32 loop
      Add (Get_Long (I * 32));
   end loop;
   Finalize (Computed);
   Verify (Actual, Computed);
--  OK, without partical word alignment the longs work too...

--  OK, now for the acid test.  Do part word operations all work correctly?

--  We will force all alignments on byte operations.
   Put_Line ("   Testing Byte offsets:");
   for I in 0 .. Test_Index (8 - 1) loop
      Put ("      Testing byte with offset of ");
      Put (Integer'Image (Integer (I)));
      Put (" ...");
      Initialize;
      for J in 0 .. I - 1 loop
         Set_Bit (Buffer (J));
      end loop;
      for J in 0 .. Test_Index'Last / 8 - 1 loop
         Add (Get_Byte (I  +  J * 8));
      end loop;
      for J in Test_Index'Last - Test_Index (8 - I) + 1 .. Test_Index'Last loop
         Set_Bit (Buffer (J));
      end loop;
      Finalize (Computed);
      Verify (Actual, Computed);
   end loop;

--  And word offsets.
   Put_Line ("   Testing Word Alignments:");
   for I in 0 .. Test_Index (16 - 1) loop
      Put ("      Testing word with offset of ");
      Put (Integer'Image (Integer (I)));
      Put (" ...");
      Initialize;
      for J in 0 .. I - 1 loop
         Set_Bit (Buffer (J));
      end loop;
      for J in 0 .. Test_Index'Last / 16 - 1 loop
         Add (Get_Word (I  +  J * 16));
      end loop;
      for J in Test_Index'Last - Test_Index (16 - I) + 1 ..
               Test_Index'Last loop
         Set_Bit (Buffer (J));
      end loop;
      Finalize (Computed);
      Verify (Actual, Computed);
   end loop;
--  And long word offsets
   Put_Line ("   Testing Long Word Alignments:");
   for I in 0 .. Test_Index (32 - 1) loop
      Put ("      Testing Long Word with offset of ");
      Put (Integer'Image (Integer (I)));
      Put (" ...");
      Initialize;
      for J in 0 .. I - 1 loop
         Set_Bit (Buffer (J));
      end loop;
      for J in 0 .. Test_Index'Last / 32 - 1 loop
         Add (Get_Long (I  +  J * 32));
      end loop;
      for J in Test_Index'Last - Test_Index (32 - I) + 1 ..
               Test_Index'Last loop
         Set_Bit (Buffer (J));
      end loop;
      Finalize (Computed);
      Verify (Actual, Computed);
   end loop;



   --  -- -------------------   Cleanup. -----------------


   Put ("SHA.Process_Data test ");
   if Result = Success then
      Put ("Passed!");
   else
      Put ("*** FAILED ****");
   end if;
   New_Line;

   Set_Exit_Status (Result);

end SHA_Process_Data_TEST;
