with Interfaces;

package Utils is
   function String_to_U32 (Str : String) return Interfaces.Unsigned_32;
   function Byte_To_Hex (Value : Interfaces.Unsigned_8) return String;
   function U32_to_Hex (Value : Interfaces.Unsigned_32) return String;
end Utils;
