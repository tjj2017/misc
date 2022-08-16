with Types;
use type Types.Node_Id;
package body SPARK_Classic.Symbols.Node_Lists
  with Refined_State => (In_Progress => In_Progress_Flag)
is
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

   procedure Set_Branch (Is_Right : Boolean;
                         Node     : in out List_Store.Store_Node;
                         Index    : Table_Index);

   procedure Set_Branch (Is_Right : Boolean;
                         Node     : in out List_Store.Store_Node;
                         Index    : Table_Index) is
   begin
      if Is_Right then
         Node.Right := Index;
      else
         Node.Left := Index;
      end if;

   end Set_Branch;
   pragma Inline (Set_Branch);

   procedure Skew (Root : in out Table_Index)
     --  --# global in out List_Store.Store;
     with Global => (In_Out => List_Store.Store);

   procedure Skew (Root : in out Table_Index) is
      Root_Node : List_Store.Store_Node :=
        List_Store.Item (Root);
      Left_Child_Index : constant Table_Index := Root_Node.Left;
      Left_Child_Node : List_Store.Store_Node :=
        List_Store.Item (Left_Child_Index);
   begin
      --  No action is performed if the levels of the root and left nodes
      --  are not equal.
      if Left_Child_Node.Level = Root_Node.Level then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Root_Node.Left := Left_Child_Node.Right;
         Left_Child_Node.Right := Root;
         --  Update the root node and the left child node in the list store.
         List_Store.Set_Item (Root, Root_Node);
         List_Store.Set_Item (Left_Child_Index, Left_Child_Node);
         --  The root now becomes the left child.
         Root := Left_Child_Index;
      end if;
   end Skew;

   procedure Split (Root : in out Table_Index)
     --  --# global in out List_Store.Store;
     with Global => (In_Out => List_Store.Store);

   procedure Split (Root : in out Table_Index) is
      Root_Node : List_Store.Store_Node :=
        List_Store.Item (Root);
      Right_Child_Index : constant Table_Index := Root_Node.Right;
      Right_Child_Node : List_Store.Store_Node :=
        List_Store.Item (Right_Child_Index);
      Right_Right_Child_Node : List_Store.Store_Node :=
        List_Store.Item (Right_Child_Node.Right);
   begin
      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Right_Right_Child_Node.Level = Root_Node.Level then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the ruight child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.

         Root_Node.Right := Right_Child_Node.Left;
         Right_Child_Node.Left := Root;
         Right_Child_Node.Level := Right_Child_Node.Level + 1;
         --  Update the root node and right child node in the list store.
         List_Store.Set_Item (Root, Root_Node);
         List_Store.Set_Item (Right_Child_Index, Right_Child_Node);
         --  The root noe becomes the right child.
         Root := Right_Child_Index;
      end if;
   end Split;


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
         Do_Insert : declare
            --  The Current_Index is initially set to the root of the tree.
            Current_Root  : Table_Index := List.Root;
            --  The Current_Node is a copy of the Store_Node at
            --  the Current_Index
            Current_Node  : List_Store.Store_Node :=
              List_Store.Item (Current_Root);
            --  A stack to record visited nodes.
            --  A adding a new node does not count as visiting that node.
            --  A new node will always be leaf and does not need to be
            --  processed when working back up the tree.
            --  Hence, a stack depth equal to the Current_Root leve should
            --  always be sufficient.
            Visited       : Stack (1 .. Current_Node.Level);
            Top           : Natural := 0;
            Top_Index     : Table_Index;
            --  Direction: Left = False, Right = True
            Dir           : Boolean;

            --  A Child index of the current node.
            Child         : Table_Index;
         begin
            --  Search the binary tree to locate an appropriate leaf to
            --  place the value of N.
            loop
               --  A record of nodes visited is held in the stack
               --  and Top is locates the current top of the stack.
               Top := Top + 1;
               Visited (Top) := Current_Root;

               --  Take the right branch if the vale of N is greater (or equal)
               --  to the Current_Node Value, otherwise take the left branch.
               Dir := Current_Node.Value < N;
               Child := Get_Child (Dir, Current_Node);

               exit when Child = No_Index;

               --  Traverse the tree: the Current_Index is set to one of its
               --  children.
               Current_Root := Child;
               Current_Node := List_Store.Item (Current_Root);
            end loop;

            --  A leaf node has been reached in the search.
            --  Add a new child node to extend the tree
            List_Store.Append (New_Node (N     => N,
                                         Level => 1));
            Set_Branch (Is_Right => Dir,
                        Node     => Current_Node,
                        Index    => List_Store.Last);

            --  Now rebalance the tree by working back up through the visited
            --  node indices on the stack, Up.
            for Stack_Top in reverse Natural range 1 .. Top loop
               --  Make the Current_Root equal to the the index at the top of
               --  the stack and the Current_Node equal to the node selected
               --  by the Current_Root.
               Top_Index := Visited (Stack_Top);
               Current_Root := Top_Index;

               --  Perform the Anderson Tree Skew and Split operations on
               --  the Current_Root.  The node which is the Current_Root
               --  may be changed by Skew and Split and so the Index contained
               --  in Current_Root may change.
               Skew (Current_Root);
               Split (Current_Root);

               --  Check if the value of the Current_Root has changed.
               if Current_Root /= Top_Index and then Stack_Top /= 1 then
                  --  The value of the Current_Root
                  --  has changed and the stack will have a
                  --  direct ancestor of the Current_Root in Visited (Top - 1).
                  --  The branch that has the Current_Root as its child
                  --  has to be patched up to contain the new value of
                  --  Current_Root as its child.
                  --  As the value of the Current_Root may have changed
                  Patch_Ancestor_Node :
                  declare
                     Ancestor_Index  : constant Table_Index :=
                       Visited (Stack_Top - 1);
                     Direct_Ancestor : List_Store.Store_Node :=
                       List_Store.Item (Ancestor_Index);
                  begin
                     --  This boolean expression determines which branch of
                     --  the direct ancestor has the original root index
                     --  as its child.
                     --  False => Left, True => Right.
                     Dir := Direct_Ancestor.Left = Top_Index;
                     Set_Branch
                       (Is_Right => Dir,
                        Node     => Direct_Ancestor,
                        Index    => Ancestor_Index);
                  end Patch_Ancestor_Node;
               end if;
             end loop;

            --  The root of the tree after inserting a node and rebalancing.
            List.Root := Current_Root;
         end Do_Insert;
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

   function New_Enumerator (N_List : Node_List) return Types.Node_Id is
   begin
      return Enumerator'
        (Root  => N_List,
         Index => N_List.Root);
   end New_Enumerator;

   function Next (E : Enumerator) return Types.Node_Id is
      Next_Node : constant List_Store.Store_Node :=
        List_Store.Item ( E.Index);
      Result : Types.Node_Id;
   begin
      if  then
         Result := Types.Empty;
      else

   -- --#  global List_Store.Store;
   with Global => List_Store.Store;
end SPARK_Classic.Symbols.Node_Lists;
