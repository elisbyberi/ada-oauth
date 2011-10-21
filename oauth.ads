with Ada.Strings.Unbounded;

with Hauki.Containers.Doubly_Linked_Lists;

use Ada.Strings.Unbounded;

package OAuth is
   type String_Pair is record
      Key   : Unbounded_String := Null_Unbounded_String;
      Value : Unbounded_String := Null_Unbounded_String;
   end record;

   function Compare_Pairs (Left, Right : String_Pair) return Boolean;

   package Parameter_List is
     new Hauki.Containers.Doubly_Linked_Lists (String_Pair);

   package Parameter_Sorting is
     new Parameter_List.Generic_Sorting (Compare_Pairs);

   function URL_Encode (Str : String) return String;

   function Create_Parameter_List (Key_1   : String;
                                   Value_1 : String;
                                   Key_2   : String;
                                   Value_2 : String)
     return Parameter_List.List;

   function Create_Parameter_List (Consumer_Key     : String;
                                   Signature_Method : String;
                                   Timestamp        : String;
                                   Nonce            : String;
                                   Version          : String := "")
     return Parameter_List.List;

   function Create_Parameter_List (Consumer_Key     : String;
                                   Token            : String;
                                   Signature_Method : String;
                                   Timestamp        : String;
                                   Nonce            : String;
                                   Version          : String := "")
     return Parameter_List.List;

   function Create_Base_String (Method             : String;
                                URL                : String;
                                Request_Parameters : Parameter_List.List)
     return String;

   function Params_To_String (Request_Parameters : Parameter_List.List)
     return String;

   function Params_To_Header (Request_Parameters : Parameter_List.List)
     return String;
   -- Turn parameters into OAuth header.

   function Nonce (Source_1 : String; Source_2 : String;
                   Source_3 : String := ""; Source_4 : String := "")
     return String;

   function Timestamp return String;
end OAuth;
