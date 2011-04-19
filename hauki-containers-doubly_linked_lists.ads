--
-- Copyright (c) 2006-2008 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ada.Finalization;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type)
     return Boolean is <>;
package Hauki.Containers.Doubly_Linked_Lists is
   -- pragma Preelaborate(Doubly_Linked_Lists);

   type Cursor is private;

   List_Empty : exception;
   Out_Of_Range : exception;
   Invalid_Cursor : exception;

   type List is tagged private;
   -- List is a Controlled type. You can safely copy it.
   -- Although, notice that if you have a list of pointers (access types),
   -- only the pointers are copied, not the objects they point at.

   Empty_List : constant List;
   -- A list with zero elements.
   -- For example, can be used for initialization.

   No_Element : constant Cursor;

   function "=" (Left, Right : List) return Boolean;
   -- Compares the lists.
   --
   -- Time: O(N)

   function Length (Container : List) return Count_Type;
   -- Return the size of the list.
   --
   -- Time: O(1)

   function Is_Empty (Container : List) return Boolean;
   -- Is the list empty?
   --
   -- Time: O(1)

   procedure Clear (Container : in out List);
   -- Remove all elements from the list.
   --
   -- Time: O(N)

   function Element (Position : Cursor) return Element_Type;
   -- Return element pointed by the iterator.
   --
   -- Time: O(1)

   procedure Replace_Element (Container : in out List;
                              Position  :        Cursor;
                              New_Item  :        Element_Type);
   -- Replace the element poited by the iterator with the given item.
   --
   -- Time: O(1)

   type Query_Proc is access procedure (Element : Element_Type);

   procedure Query_Element
     (Position : Cursor;
      Process  : Query_Proc);
   -- Call the given procedure with the element pointed by the cursor.
   --
   -- Time: O(1)

   type Update_Proc is access procedure (Element : in out Element_Type);

   procedure Update_Element
     (Container : in out List;
      Position  :        Cursor;
      Process   :        Update_Proc);
   -- Call the given procedure with the element pointed by the cursor.
   -- The process is expected to modify the given element.
   --
   -- Time: O(1)

   procedure Assign (Target : in out List;
                     Source :        List);
   -- Assign the contents of the source to the Target.
   -- If Target and Source denote the same object,
   -- the procedure does nothing.
   --
   -- Time: O(N)

   function Copy (Source : List) return List;
   -- Returns a copy of Source.
   --
   -- Time: O(N)

   procedure Move (Target : in out List; Source : in out List);
   -- Move all elements from the Source list to the Target list.
   -- The Target list is cleared before move.
   --
   -- Time: O(1)

   procedure Insert (Container : in out List;
                     Before    :        Cursor;
                     New_Item  :        Element_Type;
                     Count     :        Count_Type := 1);
   -- Insert an item before the cursor.
   --
   -- Time: O(1) or actually O(Count)

   procedure Insert (Container : in out List;
                     Before    :        Cursor;
                     New_Item  :        Element_Type;
                     Position  :    out Cursor;
                     Count     :        Count_Type := 1);
   -- Insert one or more items before the cursor.
   -- Set Position to point to the first element.
   --
   -- Time: O(1) or actually O(Count)

--    procedure Insert (Container : in out List;
--                      Before    : in     Cursor;
--                      Position  :    out Cursor;
--                      Count     : in     Count_Type := 1);

   procedure Append (Container : in out List;
                     New_Item  :        Element_Type;
                     Count     :        Count_Type := 1);
   -- Append an element(s) at the end of the list.
   --
   -- Time: O(1)

   procedure Prepend (Container : in out List;
                      New_Item  :        Element_Type;
                      Count     :        Count_Type := 1);
   -- Prepend an element at the beginning of the list.
   --
   -- Time: O(1)

   procedure Delete (Container : in out List;
                     Position  :        Cursor;
                     Count     :        Count_Type := 1);
   -- Remove an elemenent pointed by the iterator.
   --
   -- Time: O(1)

   procedure Delete_First (Container : in out List);
   -- Remove the first element from the list.
   --
   -- Time: O(1)

   procedure Delete_Last  (Container : in out List);
   -- Remove the last element from the list.
   --
   -- Time: O(1)

   procedure Swap (Container : in out List;
                   I, J      :        Cursor);
   -- Swap elements pointed by I and J
   --
   -- Time: O(1)

   procedure Swap_Links (Container : in out List;
                         I, J      : in     Cursor);
   -- Swap nodes I and J.
   --
   -- Time: O(1)

   procedure Splice (Target   : in out List;
                     Before   : in     Cursor;
                     Source   : in out List;
                     Position : in out Cursor);
   -- Move element designated by Position from Source to Target and
   -- place them in front of element pointed by Before.
   -- Update Position to point to the node in Target.

   procedure Splice (Target   : in out List;
                     Before   : in     Cursor;
                     Source   : in out List);
   -- Move all elements from Source to Target and
   -- place them in front of element pointed by Before.

   function First (Container : List) return Cursor;
   -- Return an iterator to the first element of the list.
   --
   -- Time: O(1)

   function First_Element (Container : List) return Element_Type;
   -- Return the first element of the list.
   --
   -- Time: O(1)

   function Last (Container : List) return Cursor;
   -- Return an iterator to the last element of the list.
   --
   -- Time: O(1)

   function Next (Position : Cursor) return Cursor;
   -- Move the iterator to point to the next element on the list.
   --
   -- Time: O(1)

   procedure Next (Position : in out Cursor);
   -- Move the iterator to point to the next element on the list.
   --
   -- Time: O(1)

   function Previous (Position : Cursor) return Cursor;
   -- Move the iterator to point to the previous element on the list.
   --
   -- Time: O(1)

   procedure Previous (Position : in out Cursor);
   -- Move the iterator to point to the previous element on the list.
   --
   -- Time: O(1)

   function Find (Container : List;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      return Cursor;
   -- Find element from the list.
   --
   -- Time: O(N)

   function Contains (Container : List;
                      Item      : Element_Type) return Boolean;
   -- Return True if Container contains the specified Item.
   --
   -- Time: O(N)

   function Has_Element (Position : Cursor) return Boolean;
   -- Return True if Position points to a valid Element.
   --
   -- Time: O(1)

   type Iterate_Proc is access procedure (Position : Cursor);

   procedure Iterate (Container : List; Process : Iterate_Proc);
   -- Iterate through the Container and call Process for each element.

   procedure Reverse_Iterate (Container : List; Process : Iterate_Proc);
   -- Iterate through the Container in reverse order
   -- and call Process for each element.

   generic
      with function "<" (Left, Right : Element_Type)
         return Boolean is <>;
   package Generic_Sorting is
      function Is_Sorted (Container : List) return Boolean;
      procedure Sort (Container : in out List);
      procedure Merge (Target  : in out List;
                       Source  : in out List);
   end Generic_Sorting;
private
   type Node;
   type Node_Access is access Node;
   type Cursor is record
      Ptr : Node_Access;
      First_Item : Node_Access;
   end record;

   function Cursor_In_Container (Container : List; Position : Cursor)
     return Boolean;

   function Create_Nodes (Count : Count_Type; New_Item : Element_Type)
     return Node_Access;

   procedure Remove (Ptr : Node_Access);
   -- A procedure to release memory pointed by Ptr.

   type Node is record
      Data : Element_Type;
      Next : Node_Access := null;
      Prev : Node_Access := null;
   end record;

   type List is new Ada.Finalization.Controlled with record
      First : Node_Access := null;
      Last  : Node_Access := null;
      Size  : Count_Type := 0;
   end record;

   procedure Initialize (Object : in out List);
   procedure Finalize   (Object : in out List);
   procedure Adjust     (Object : in out List);

   Empty_List : constant List :=
     (Ada.Finalization.Controlled with First => null,
                                       Last  => null,
                                       Size  => 0);

   No_Element : constant Cursor := (Ptr => null, First_Item => null);
end Hauki.Containers.Doubly_Linked_Lists;
