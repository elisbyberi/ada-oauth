--  (C) Copyright 2000 by John Halleck, All Rights Reserved.
--  Tests of the SHA.strings package.
--  This code is part of a project at http://www.cc.utah.edu/~nahaj/

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with Interfaces;       use Interfaces;
with SHA;              use SHA;
with SHA.Strings;      use SHA.Strings;


procedure SHA_Strings_TEST is

   Reference : Digest;

   Hex_Result : Hex_SHA_String;
   B64_Result : B64_SHA_String;

   Hex_Expect : Hex_SHA_String;
   B64_Expect : B64_SHA_String;

   Hex_Zero : constant Character := '0';
   B64_Zero : constant Character := 'A';

   Hex_Bit_Position : constant array (0 .. 4 - 1) of Character :=
                      ('8', '4', '2', '1');
   B64_Bit_Position : constant array (0 .. 6 - 1) of Character :=
                      ('g', 'Q', 'I', 'E', 'C', 'B');

   All_Tests : Exit_Status := Success;
   This_Test : Exit_Status := Success;

   Word : Integer range Digest'Range;
   Bit  : Integer range 0 .. Bits_In_Word;
   Temp : Unsigned_32;

   Hex_Byte : Integer range Hex_SHA_String'Range;
   Hex_Form : Character;

   B64_Byte : Integer range B64_SHA_String'Range;
   B64_Form : Character;

begin

   Put_Line ("SHA_Strings test start...");


   Put ("   Testing Hex     Digest ...");
   This_Test := Success;

   for I in 0 .. Bits_In_Digest loop -- For each bit in the digest.
      Reference  := (others => 0);
      --  Form the expected value.
      Hex_Expect := (others => Hex_Zero);
      if I /= Bits_In_Digest then
         --  We skip this for bit 160, so that we can have a Zero Digest Test.
         Word := I  /  Bits_In_Word;
         Bit  := I mod Bits_In_Word;
         --  Force a left to right bit ordering.
         Temp := Rotate_Right (1, Bit + 1);
         Reference (Word) := Temp;
         Hex_Byte := I / 4 + 1;
         Hex_Form := Hex_Bit_Position (I mod 4);
         Hex_Expect (Hex_Byte) := Hex_Form;
      end if;
      Hex_Result := Hex_From_SHA (Reference);
      if Hex_Expect /= Hex_Result then
         if This_Test = Success then
            New_Line;
            This_Test := Failure;
            All_Tests := Failure;
         end if;
         Put ("      Hex test of Bit"); Put (Integer'Image (I));
         Put_Line (" failed");
         Put_Line ("      Expected: "); Put (String (Hex_Expect));
         Put_Line ("           Got: "); Put (String (Hex_Result));
      end if;
   end loop;
   if This_Test = Success then
      Put_Line (" PASSED!");
   end if;



   Put ("   Testing Base_64 Digest ...");
   This_Test := Success;

   for I in 0 .. Bits_In_Digest loop -- For each bit in the digest.
      Reference  := (others => 0);
      --  Form the expected value.
      B64_Expect := (B64_SHA_String'Last => '=', others => B64_Zero);
      if I /= Bits_In_Digest then
         Word := I  /  Bits_In_Word;
         Bit  := I mod Bits_In_Word;
         --  Force a left to right bit ordering.
         Temp := Rotate_Right (1, Bit + 1);
         Reference (Word) := Temp;
         B64_Byte := I / 6 + 1;
         B64_Form := B64_Bit_Position (I mod 6);
         B64_Expect (B64_Byte) := B64_Form;
      end if;
      B64_Result := B64_From_SHA (Reference);
      if B64_Expect /= B64_Result then
         if This_Test = Success then
            New_Line;
            This_Test := Failure;
            All_Tests := Failure;
         end if;
         Put ("      Base 64 test of Bit"); Put (Integer'Image (I));
         Put_Line (" failed");
         Put ("      Expected: "); Put_Line (String (B64_Expect));
         Put ("           Got: "); Put_Line (String (B64_Result));
      end if;
   end loop;
   if This_Test = Success then
      Put_Line (" PASSED!");
   end if;

   Put ("SHA_Strings test ");
   if All_Tests = Success then
      Put ("--- PASSED! ---");
   else
      Put ("*** FAILED ***");
   end if;
   New_Line;

   Set_Exit_Status (All_Tests);

end SHA_Strings_TEST;
