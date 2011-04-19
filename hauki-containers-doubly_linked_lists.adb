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

with Ada.Unchecked_Deallocation;

package body Hauki.Containers.Doubly_Linked_Lists is

   function Cursor_In_Container (Container : List; Position : Cursor)
     return Boolean
   is
   begin
      return Position.First_Item = Container.First;
   end Cursor_In_Container;

   function Create_Nodes (Count : Count_Type; New_Item : Element_Type)
     return Node_Access is
      Current : Node_Access := null;
      First   : Node_Access := null;
      Counter : Count_Type  := 0;
   begin
      if Count = 0 then
         return null;
      end if;

      loop
         exit when Counter = Count;
         Current :=
           new Node'(Prev => Current, Next => null, Data => New_Item);
         if First = null then
            First := Current;
         else
            Current.Prev.Next := Current;
         end if;
         First.Prev := Current;
         Counter := Counter + 1;
      end loop;
      return First;
   exception
      when Storage_Error =>
         -- Storage error, removing allocated nodes
         Current := First;
         loop
            exit when Current = null;
            First := Current.Next;
            Remove (First);
            Current := First;
         end loop;
         raise;
   end Create_Nodes;

   procedure Remove (Ptr : Node_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);
      My_Ptr : Node_Access := Ptr;
   begin
      Ptr.Next := null;
      Ptr.Prev := null;
      Free (My_Ptr);
   end Remove;

   function "=" (Left, Right : List) return Boolean is
      Left_C, Right_C : Cursor;
   begin
      if Left.Size /= Right.Size then
         return False;
      end if;

      Left_C := First (Left);
      Right_C := First (Right);

      loop
         exit when Left_C = No_Element or Right_C = No_Element;
         if Left_C.Ptr.Data /= Right_C.Ptr.Data then
            return False;
         end if;

         Next (Left_C);
         Next (Right_C);
      end loop;

      return True;
   end "=";

   procedure Delete (Container : in out List;
                     Position  :        Cursor;
                     Count     :        Count_Type := 1) is
      Current     : Node_Access;
      First_Valid : Node_Access := Container.First;
      Prev_Valid  : Node_Access := Container.First;
      Counter     : Count_Type  := 0;
   begin
      if Container.Size = 0 then
         raise Program_Error;
      elsif Position = No_Element then
         raise Constraint_Error;
      elsif Count = 0 then
         return;
      end if;

      Current := Position.Ptr;

      if Current = Container.First then
         loop
            exit when Counter = Count or Current = null;
            First_Valid := Current.Next;
            Remove (Current);
            Current := First_Valid;
            Counter := Counter + 1;
         end loop;
         Container.First := First_Valid;
         if Container.First = null then
            Container.Last := null;
         else
            Container.First.Prev := null;
         end if;
      elsif Current = Container.Last then
         Current := Container.Last;
         Container.Last := Current.Prev;
         Container.Last.Next := null;
         Remove (Current);
         Counter := 1;
      else
         Prev_Valid := Current.Prev;
         loop
            exit when Counter = Count or Current = null;
            First_Valid := Current.Next;
            Remove (Current);
            Current := First_Valid;
            Counter := Counter + 1;
         end loop;
         Prev_Valid.Next := First_Valid;
         if First_Valid /= null then
            First_Valid.Prev := Prev_Valid;
         else
            Container.Last := Prev_Valid;
         end if;
      end if;

      Container.Size := Container.Size - Counter;
   end Delete;

   procedure Insert (Container : in out List;
                     Before    :        Cursor;
                     New_Item  :        Element_Type;
                     Count     :        Count_Type := 1) is
      New_Position : Cursor;
   begin
      Insert (Container, Before, New_Item, New_Position, Count);
   end Insert;

   procedure Insert (Container : in out List;
                     Before    :        Cursor;
                     New_Item  :        Element_Type;
                     Position  :    out Cursor;
                     Count     :        Count_Type := 1) is
      New_Node : Node_Access;
      Last_New_Node : Node_Access;
   begin
      if Count = 0 then
         Position := No_Element;
         return;
      end if;

      if Before /= No_Element and
        not Cursor_In_Container (Container, Before) then
         raise Program_Error;
      end if;

      New_Node := Create_Nodes (Count, New_Item);
      -- New_Node.Prev points to the last of the created nodes.
      Last_New_Node := New_Node.Prev;
      New_Node.Prev := null;

      -- Note: Order of the assignments matter!
      -- List empty?
      if Container.First = null then
         Container.First := New_Node;
         Container.Last := Last_New_Node;
         Container.First.Prev := null;
      -- Inserting at the beginning of the list?
      elsif Before.Ptr = Container.First then
         Last_New_Node.Next := Container.First;
         Container.First.Prev := Last_New_Node;
         Container.First := New_Node;
         Container.First.Prev := null;
      -- Inserting at the end of the list?
      elsif Before.Ptr = null then
         Container.Last.Next := New_Node;
         New_Node.Prev := Container.Last;
         Container.Last := Last_New_Node;
      -- Inserting in the middle of the list
      else
         declare
            Previous_Node : Node_Access := Before.Ptr.Prev;
         begin
            Previous_Node.Next := New_Node;
            Before.Ptr.Prev := Last_New_Node;
            Last_New_Node.Next := Before.Ptr;
            New_Node.Prev := Previous_Node;
         end;
      end if;
      Position := (Ptr => New_Node, First_Item => Container.First);
      Container.Size := Container.Size + Count;
   end Insert;

--    procedure Insert (Container : in out List;
--                      Before    : in     Cursor;
--                      Position  :    out Cursor;
--                      Count     : in     Count_Type := 1) is
--       New_Element : Element_Type := Element_Type'(Element_Type);
--    begin
--       Insert (Container => Container,
--               Before    => Before,
--               New_Item  => New_Element,
--               Position  => Position,
--               Count     => Count);
--    end Insert;

   procedure Prepend (Container : in out List;
                      New_Item  :        Element_Type;
                      Count     :        Count_Type := 1) is
   begin
      Insert (Container, First (Container), New_Item, Count);
   end Prepend;

   procedure Append (Container : in out List;
                     New_Item  :        Element_Type;
                     Count     :        Count_Type := 1) is
   begin
      Insert (Container, No_Element, New_Item, Count);
   end Append;

   procedure Clear (Container : in out List) is
      Current_Node : Node_Access := Container.First;
      Next_Node : Node_Access := null;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Remove (Current_Node);
         Current_Node := Next_Node;
      end loop;

      Container.First := null;
      Container.Last := null;
      Container.Size := 0;
   end Clear;

   procedure Delete_First (Container : in out List) is
      Temp_Node : Node_Access;
   begin
      if Container.Size = 0 then
         raise List_Empty;
      end if;

      Temp_Node := Container.First;
      Container.First := Container.First.Next;
      if Container.First /= null then
         Container.First.Prev := null;
      else
         Container.Last := null;
      end if;

      Remove (Temp_Node);
      Container.Size := Container.Size - 1;
   end Delete_First;

   procedure Delete_Last (Container : in out List) is
      Temp_Node : Node_Access;
   begin
      if Container.Size = 0 then
         raise List_Empty;
      end if;

      Temp_Node := Container.Last;

      Container.Last := Temp_Node.Prev;
      if Container.Last /= null then
         Container.Last.Next := null;
      else
         Container.First := null;
      end if;

      Remove (Temp_Node);
      Container.Size := Container.Size - 1;
   end Delete_Last;

   procedure Splice (Target   : in out List;
                     Before   : in     Cursor;
                     Source   : in out List;
                     Position : in out Cursor) is
      Removable : Node_Access;
      Prev_Node : Node_Access;
      Next_Node : Node_Access;
      Before_Node : Node_Access := Before.Ptr;
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Target.First = Source.First and Before = Position then
         return;
      end if;

      Removable := Position.Ptr;

      -- Remove node from Source list
      Source.Size := Source.Size - 1;
      if Removable = Source.First and Removable = Source.Last then
         Source.First := null;
         Source.Last := null;
      elsif Removable = Source.First then
         Source.First := Removable.Next;
         Source.First.Prev := null;
      elsif Removable = Source.Last then
         Source.Last := Removable.Prev;
         Source.Last.Next := null;
      else
         Prev_Node := Removable.Prev;
         Next_Node := Removable.Next;
         Prev_Node.Next := Next_Node;
         Next_Node.Prev := Prev_Node;
      end if;

      -- Add node to the Target list
      Target.Size := Target.Size + 1;
      if Target.Size = 1 then
         Removable.Prev := null;
         Removable.Next := null;
         Target.First := Removable;
         Target.Last := Removable;
      elsif Before = No_Element then
         Removable.Prev := Target.Last;
         Removable.Next := null;
         Target.Last.Next := Removable;
         Target.Last := Removable;
      elsif Before_Node = Target.First then
         Removable.Prev := null;
         Removable.Next := Target.First;
         Target.First.Prev := Removable;
         Target.First := Removable;
      else
         Removable.Prev := Before_Node.Prev;
         Removable.Next := Before_Node;
         Before_Node.Prev.Next := Removable;
         Before_Node.Prev := Removable;
      end if;
      Position := (Ptr => Removable, First_Item => Target.First);
   end Splice;

   procedure Splice (Target   : in out List;
                     Before   : in     Cursor;
                     Source   : in out List) is
      New_Node : constant Node_Access := Source.First;
   begin
      if Source.Size = 0 or Source.First = Target.First then
         return;
      end if;
      if Before = No_Element then
         if Target.Last = null then
            Target.First := New_Node;
         else
            Target.Last.Next := New_Node;
            New_Node.Prev := Target.Last;
         end if;
         Target.Last := Source.Last;
      else
         -- Target list is empty?
         if Target.First = null then
            Target.First := New_Node;
            Target.Last := Source.Last;
            Target.First.Prev := null;
         -- We insert at the beginning of the list?
         elsif Before.Ptr = Target.First then
            Source.Last.Next := Target.First;
            Target.First.Prev := Source.Last;
            Target.First := New_Node;
            Target.First.Prev := null;
         else
            -- New order will be:
            -- [previous_node] [new_node] ... [source.last] [target.first]
            declare
               Previous_Node : Node_Access := Before.Ptr.Prev;
            begin
               Previous_Node.Next := New_Node;
               Before.Ptr.Prev := Source.Last;
               Source.Last.Next := Before.Ptr;
               New_Node.Prev := Previous_Node;
            end;
         end if;
      end if;
      Target.Size := Target.Size + Source.Size;

      Source.First := null;
      Source.Last := null;
      Source.Size := 0;
   end Splice;

   procedure Replace_Element (Container : in out List;
                              Position  :        Cursor;
                              New_Item  :        Element_Type) is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;

      if not Cursor_In_Container (Container, Position) then
         raise Program_Error;
      end if;

      Position.Ptr.Data := New_Item;
   end Replace_element;

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Size = 0;
   end Is_Empty;

   function First (Container : List) return Cursor is
   begin
      if Container.Size = 0 then
         return (Ptr => null, First_Item => null);
      end if;

      return (Ptr => Container.First, First_Item => Container.First);
   end First;

   function First_Element (Container : List) return Element_Type is
   begin
      return Container.First.Data;
   end First_Element;

   function Last (Container : List) return Cursor is
   begin
      if Container.Size = 0 then
         return (Ptr => null, First_Item => null);
      end if;

      return (Ptr => Container.Last, First_Item => Container.First);
   end Last;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Ptr = null or else Position.Ptr.Next = null then
         return No_Element;
      end if;
      return (Ptr        => Position.Ptr.Next,
              First_Item => Position.First_Item);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Ptr = null or else Position.Ptr.Prev = null then
         return No_Element;
      end if;
      return (Ptr        => Position.Ptr.Prev,
              First_Item => Position.First_Item);
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Find (Container : List;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
     return Cursor is

      Current : Node_Access := null;
   begin
      if Position.Ptr = null then
         Current := Container.First;
      else
         Current := Position.Ptr;
      end if;
      loop
         exit when Current = null;
         if Current.Data = Item then
            return (Ptr => Current, First_Item => Container.First);
         end if;
         Current := Current.Next;
      end loop;

      return No_Element;
   end Find;

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      return Position.Ptr.Data;
   end Element;

   function Length (Container : List) return Count_Type is
   begin
      return Container.Size;
   end Length;

   procedure Move (Target : in out List; Source : in out List) is
   begin
      Clear (Target);
      if Source.Size = 0 then
         return;
      end if;

      Target.First := Source.First;
      Target.Last := Source.Last;
      Target.Size := Source.Size;

      -- No need to release Source's memory
      -- because all nodes are transferred to Target
      Source.Last := null;
      Source.First := null;
      Source.Size := 0;
   end Move;

   procedure Initialize (Object : in out List) is
   begin
      Object.Last := null;
      Object.First := null;
      Object.Size := 0;
   end Initialize;

   procedure Finalize (Object : in out List) is
   begin
      Clear (Object);
   end Finalize;

   procedure Adjust (Object : in out List) is
      Target_Last : Node_Access := null;
      Target_First : Node_Access := null;
      Current : Node_Access := Object.First;
      New_Node : Node_Access;
   begin
      while Current /= null loop
         New_Node := new Node'(Data => Current.Data,
           Next => null, Prev => Target_Last);

         if Target_Last = null then
            Target_Last := New_Node;
            Target_First := New_Node;
         else
            Target_Last.Next := New_Node;
            Target_Last := New_Node;
         end if;

         Current := Current.Next;
      end loop;
      Object.First := Target_First;
      Object.Last := Target_Last;
   end Adjust;

   procedure Query_Element
     (Position : Cursor;
      Process  : Query_Proc) is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      end if;
      Process.all (Position.Ptr.all.Data);
   end Query_Element;

   procedure Update_Element
     (Container : in out List;
      Position  :        Cursor;
      Process   :        Update_Proc) is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Container.First /= Position.First_item then
         raise Program_Error;
      end if;

      Process.all (Position.Ptr.all.Data);
   end Update_Element;

   procedure Assign (Target : in out List;
                     Source :        List) is
   begin
      if Target.First = Source.First then
         return;
      end if;

      Target := Source;
   end Assign;

   function Copy (Source : List) return List is
   begin
      return Source; -- XXX is this enough or should we
                     -- create a temporary copy also.
   end Copy;

   procedure Swap (Container : in out List;
                   I, J      :        Cursor) is
      Temp : Element_Type;
   begin
      if I = No_Element or J = No_Element then
         raise Constraint_Error;
      elsif I.First_Item /= Container.First or
            J.First_Item /= Container.First then
         raise Program_Error;
      elsif I.Ptr = J.Ptr then
         return;
      end if;

      Temp := I.Ptr.Data;
      I.Ptr.Data := J.Ptr.Data;
      J.Ptr.Data := Temp;
   end Swap;

   procedure Swap_Links (Container : in out List;
                         I, J      : in     Cursor) is
      I_Is_First : constant Boolean := I.Ptr = Container.First;
      I_Is_Last  : constant Boolean := I.Ptr = Container.Last;
      J_Is_First : constant Boolean := J.Ptr = Container.First;
      J_Is_Last  : constant Boolean := J.Ptr = Container.Last;

      Prev_Link, Next_Link : Node_Access;
   begin
      if I = No_Element or J = No_Element then
         raise Constraint_Error;
      elsif I.Ptr = J.Ptr then
         return;
      end if;

      Prev_Link := I.Ptr.Prev;
      Next_Link := I.Ptr.Next;

      if I.Ptr.Prev /= null then
         I.Ptr.Prev.Next := J.Ptr;
      end if;
      if I.Ptr.Next /= null then
         I.Ptr.Next := J.Ptr;
      end if;
      I.Ptr.Prev := J.Ptr.Prev;
      I.Ptr.Next := J.Ptr.Next;

      if J.Ptr.Prev /= null then
         J.Ptr.Prev.Next := I.Ptr;
      end if;
      if J.Ptr.Next /= null then
         J.Ptr.Next.Prev := I.Ptr;
      end if;
      J.Ptr.Prev := Prev_Link;
      J.Ptr.Next := Next_Link;

      if I_Is_First then
         Container.First := J.Ptr;
      end if;
      if I_Is_Last then
         Container.Last := J.Ptr;
      end if;
      if J_Is_First then
         Container.First := I.Ptr;
      end if;
      if J_Is_Last then
         Container.Last := I.Ptr;
      end if;
   end Swap_Links;

   function Contains (Container : List;
                      Item      : Element_Type) return Boolean is
      Current : Node_Access := Container.First;
   begin
      loop
         exit when Current = null;
         if Current.Data = Item then
            return True;
         end if;
         Current := Current.Next;
      end loop;
      return False;
   end Contains;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Ptr /= null;
   end Has_Element;

   procedure Iterate (Container : List; Process : Iterate_Proc) is
      Current : Node_Access := Container.First;
   begin
      loop
         exit when Current = null;
         Process.all (Cursor'(Current, Container.First));
         Current := Current.Next;
      end loop;
   end Iterate;

   procedure Reverse_Iterate (Container : List; Process : Iterate_Proc) is
      Current : Node_Access := Container.Last;
   begin
      loop
         exit when Current = null;
         Process.all (Cursor'(Current, Container.First));
         Current := Current.Prev;
      end loop;
   end Reverse_Iterate;

   package body Generic_Sorting is
      function Is_Sorted (Container : List) return Boolean is
         Pos : Cursor := First (Container);
         Next_Pos : Cursor;
      begin
         loop
            exit when Pos = No_Element;
            Next_Pos := Next (Pos);
            exit when Next_Pos = No_Element;
            if not (Element (Pos) < Element (Next_Pos)) then
               return False;
            end if;

            Pos := Next_Pos;
         end loop;
         return True;
      end Is_Sorted;

      procedure Sort (Container : in out List) is
         Left : List := Empty_List;
         Middle : constant Count_Type := Length (Container) / 2;
         Pos : Cursor;
      begin
         if Length (Container) < 2 then
            return;
         end if;

         for I in Count_Type range 1 .. Middle loop
            Pos := First (Container);
            Splice (Target => Left,
                    Before => No_Element,
                    Source => Container,
                    Position => Pos);
         end loop;

         Sort (Left);
         Sort (Container);
         Merge (Left, Container);
         Move (Target => Container, Source => Left);
      end Sort;

      procedure Merge (Target  : in out List;
                       Source  : in out List) is
         Result : List := Empty_List;
         Pos : Cursor;
      begin
         loop
            exit when Length (Target) = 0 or Length (Source) = 0;
            if First_Element (Target) < First_Element (Source) then
               Pos := First (Target);
               Splice (Target => Result,
                       Before => No_Element,
                       Source => Target,
                       Position => Pos);
            else
               Pos := First (Source);
               Splice (Target => Result,
                       Before => No_Element,
                       Source => Source,
                       Position => Pos);
            end if;
         end loop;

         if Length (Target) > 0 then
            Splice (Target => Result,
                    Before => No_Element,
                    Source => Target);
         else
            Splice (Target => Result,
                    Before => No_Element,
                    Source => Source);
         end if;

         Move (Target => Target,
               Source => Result);
      end Merge;
   end Generic_Sorting;

end Hauki.Containers.Doubly_Linked_Lists;

