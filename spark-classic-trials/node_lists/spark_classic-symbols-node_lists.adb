with Types;
use type Types.Node_Id;
package body SPARK_Classic.Symbols.Node_Lists
  with Refined_State => (In_Progress => In_Progress_Flag)
is
   type Stack is array (Positive range <>) of Table_Index;

   --  In_Progress : Boolean := False;
   In_Progress_Flag : Boolean := False;

   Left  : constant Boolean := False;
   Right : constant Boolean := True;

   function New_Node (N : Types.Node_Id; Level : Positive)
                      return List_Store.Store_Node is
     (List_Store.Store_Node'
        (Level => Level,
         Left  => No_Index,
         Right => No_Index,
         Value => N));

   function Get_Child (Is_Right : Boolean; Node : List_Store.Store_Node)
                       return Table_Index is
     (if Is_Right then Node.Right else Node.Left);
   pragma Inline (Get_Child);

   procedure Set_Child (Is_Right : Boolean;
                        Node     : in out List_Store.Store_Node;
                        Index    : Table_Index);

   procedure Set_Child (Is_Right : Boolean;
                        Node     : in out List_Store.Store_Node;
                        Index    : Table_Index) is
   begin
      if Is_Right then
         Node.Right := Index;
      else
         Node.Left := Index;
      end if;

   end Set_Child;
   pragma Inline (Set_Child);

   procedure Skew (Root : in out Table_Index)
     --  --# global in out List_Store.Store;
     with Global => (In_Out => List_Store.Store);

   procedure Skew (Root : in out Table_Index) is
      Root_Node : List_Store.Store_Node :=
        List_Store.Item (Root);
      Root_Left_Child : List_Store.Store_Node :=
        List_Store.Item (Entry_Root.Left);
   begin
      if Left_Child.Level = Root.Level and thenRoot.Level /= 0 then
         declare
            Root_Right_Child : List_Store.Store_Node :=
              List_Store.Item (Entry_Root.Right);
            Save_Index : Table_Index := Root_Node.Left;
            New_Root_Node  : List_Store.Store_Node :=
              List_Store.Item (Save_Index);
         begin
            Root_Node.Left := Save_Node.Right;
            New_Root_Node.Right := Root
   end Skew;



   -------------------
   -- Building_List --
   -------------------

   function Building_List return Boolean
     with Refined_Global => In_Progress_Flag
   is
   begin
      return In_Progress_Flag;
   end Building_List;
   pragma Inline (Building_List);

   --------------
   -- New_List --
   --------------

   procedure New_List (List : out Node_List)
   is
   begin
      In_Progress_Flag := True;
      List := Null_Node_List;
   end New_List;

   ------------
   -- Insert --
   ------------

   procedure Insert (N : Types.Node_Id; List : in out Node_List)
   is
   begin
      if List.Root = No_Index then
         --  First node of list - Enter a new node with level 1 into the store
         List_Store.Append (New_Node (N     => N,
                                   Level => 1));
         --  The new node is the root of the new list
         List.Root := List_Store.Last;
      else
         Search : declare
            --  The Current_Index is initially set to the root of the tree.
            Current_Index : Table_Index := List.Root;
            --  The Current_Node is a copy of the Store_Node at
            --  the Current_Index
            Current_Node  : List_Store.Store_Node :=
              List_Store.Item (Current_Index);
            --  A stack to record visited nodes.
            Up            : Stack (1 .. Current_Node.Level);
            Top           : Natural := 0;
            --  Direction: Left = False, Right = True
            Dir           : Boolean;

            --  A Child index of the current node.
            Child         : Table_Index;
         begin
            --  Search the binary tree to locate an appropriate leaf to
            --  place the value of N.
            loop
               --  A record of nodes visited is held in the stack, Up,
               --  and Top is locates the current top of the stack.
               Top := Top + 1;
               Up (Top) := Current_Index;

               --  Take the right branch if the vale of N is greater (or equal)
               --  to the Current_Node Value, otherwise take the left branch.
               Dir := Current_Node.Value < N;
               Child := Get_Child (Dir, Current_Node);

               exit when Child = No_Index;

               --  Traverse the tree: the Current_Index is set to one of its
               --  children.
               Current_Index := Child;
               Current_Node := List_Store.Item (Current_Index);
            end loop;

            --  A leaf node has been reached in the search.
            --  Add a new child node to extend the tree
            List_Store.Append (New_Node (N     => N,
                                         Level => 1));
            Set_Child (Is_Right => Dir,
                       Node     => Current_Node,
                       Index    => List_Store.Last);

            --  Now rebalance the tree by working back up through the visited
            --  node indices on the stack, Up.
            while Top > 0 loop
               --  Make the Current_Index equal to the top of the stack and
               --  the Current_Node equal to the node selected by the
               --  Current_Index.
               Current_Index := Up (Top);
               Current_Node := List_Store.Item (Current_Index);



               --  Perform the Anderson Tree Skew and Split operations on
               --  the Current_Node.
               Skew (Current_Index);
               Split (Current_Index);

               Top := Top - 1;
            end loop;
         end Search;
      end if;
   end Insert;

   ---------------
   -- Save_List --
   ---------------

   procedure Save_List (List : in out Node_List)
   is
   begin
      In_Progress_Flag := False;
   end Save_List;

   -----------------
   --  Is_Present --
   -----------------

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
   is
   begin
      return False;
   end Is_Present;

end SPARK_Classic.Symbols.Node_Lists;
