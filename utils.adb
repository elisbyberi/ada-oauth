with Ada.Text_IO; use Ada.Text_IO;

package body Utils is
   use Interfaces;

   function String_to_U32 (Str : String) return Interfaces.Unsigned_32 is
      Value : Unsigned_32 := 0;
      Index : Natural := 3;
   begin
      for I in Str'Range loop
         Value := Value +
           Interfaces.Shift_Left (Character'Pos (Str (I)),8*Index);
         if Index > 0 then
            Index := Index - 1;
         end if;
      end loop;

      return Value;
   end String_to_U32;

   Hex_A : constant array (0 .. 15) of Character :=
    ('0', '1','2','3','4','5','6','7','8','9','a', 'b', 'c', 'd', 'e', 'f');

   function Byte_To_Hex (Value : Interfaces.Unsigned_8) return String is
      Lo : Unsigned_8;
      Hi : Unsigned_8;
   begin
      Lo := Value and 16#0F#;
      Hi := Shift_Right (Value, 4);

      return "" & Hex_A (Integer (Hi)) & Hex_A (Integer (Lo));
   end Byte_To_Hex;

   function U32_to_Hex (Value : Interfaces.Unsigned_32) return String is
      A, B, C, D : Unsigned_8;
   begin
      A := Unsigned_8 (Shift_Right (Value, 24) and 16#FF#);
      B := Unsigned_8 (Shift_Right (Value, 16) and 16#FF#);
      C := Unsigned_8 (Shift_Right (Value, 8) and 16#FF#);
      D := Unsigned_8 (Value and 16#FF#);

      return Byte_To_Hex (A) &
             Byte_To_Hex (B) &
             Byte_To_Hex (C) &
             Byte_To_Hex (D);
   end U32_to_Hex;
end Utils;
