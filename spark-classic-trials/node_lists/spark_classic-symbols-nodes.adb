with Types;
use type Types.Node_Id;
with Ada.Text_IO; use Ada.Text_IO;
package body SPARK_Classic.Symbols.Nodes
with Refined_State => (Store => (Tree_Store, In_Progress))
   --  --# own Store is Tree_Store, In_Progress;
is
   Tree_Store  : Node_Tree.Tree_Type;
   In_Progress : Boolean;

   Take_Left  : constant Boolean := False;
   Take_Right : constant Boolean := True;

   procedure Print_Stack (S : SPARK_Classic.Symbols.Node_Stacks.Stack_Type) is
   begin
      Put_Line ("Stack contents - " & Integer'Image (S.count) & " elements");
      for I in Natural range 0 .. S.Count - 1 loop
         Put (Types.Node_Id'Image (Tree_Store.Key (S.Predecessor (I)))
              & " ");
      end loop;
      New_Line;
   end Print_Stack;

   procedure Initialize_Store is
   begin
      Tree_Store.New_Tree;
      In_Progress := False;
   end Initialize_Store;

   procedure New_Node (N        : Types.Node_Id;
                       Level    : Positive;
                       The_Node : out Node_Tree.Tree_Node)
     --  --# global in out Tree_Store;
   is
   begin
      Tree_Store.Add_Node (The_Node, Node_Tree.Element_Type'(Key => N));
   end New_Node;

   function Get_Child (Is_Right : Boolean; Node : Node_Tree.Tree_Node)
                       return Node_Tree.Tree_Node
   with Global => Tree_Store;

   function Get_Child (Is_Right : Boolean; Node : Node_Tree.Tree_Node)
                       return Node_Tree.Tree_Node
   --  --# global Tree_Store;
   is
      Result : Node_Tree.Tree_Node;
   begin
      if Is_Right then
         Result := Tree_Store.Right (Node);
      else
         Result := Tree_Store.Left (Node);
      end if;
      return Result;
   end Get_Child;
   pragma Inline (Get_Child);

   procedure Set_Branch (Is_Right : Boolean;
                         Node     : Node_Tree.Tree_Node;
                         New_Node : Node_Tree.Tree_Node)
   with Global => (In_Out => Tree_Store);

   procedure Set_Branch (Is_Right : Boolean;
                         Node     : Node_Tree.Tree_Node;
                         New_Node : Node_Tree.Tree_Node)
   --  --# global Tree_Store;
   is
   begin
      if Is_Right then
         Tree_Store.Set_Right
           (N      => Node,
            Branch => New_Node);
      else
         Tree_Store.Set_Left
           (N      => Node,
            Branch => New_Node);
      end if;

   end Set_Branch;
   pragma Inline (Set_Branch);

   procedure Find (N : Types.Node_Id; List : in out Node_List;
                   Found : out Boolean)
     with Global => (Input => Tree_Store)
       --  -- # global in Tree_Store
     --  If found is true, the top of the List.Visited stack is the Tree_Node
     --  that references the The Actual_Node in the Tree_Store which
     --  contains the value of N,
   is
      Current_Root  : Node_Tree.Tree_Node;
      Current_Value : Types.Node_Id;

      --  A Child of the current node.
      Child         : Node_Tree.Tree_Node;
      --  Direction: Left = False, Right = True
      Is_Right      : Boolean;
   begin
      --  Assume that a match for node, N, has not been found.
      Found := False;
      --  If the List.Root is not present, the list is empty
      --  and the given node will not be found.
      --  There is nothing more to be done.
      List.Visited.Clear;

      if Tree_Store.Present (List.Root) then
         --  The list is not empty the given node, N, may be present.

         --  The Current_Root is initially set to the root of the tree.
         Current_Root := List.Root;
         --  Search the binary tree to find a matching value or, if none is
         --  found, locate an appropriate leaf to place the value of N.
         loop
            --  A record of nodes visited is held in the Visited stack.
            List.Visited.Push (Current_Root);
            Current_Value := Tree_Store.Key (Current_Root);
            if Current_Value = N then
               Found := True;
            else
               --  Take the right branch if the value of N is greater
               --  to the Current_Node Value, otherwise take the left branch.
               Is_Right := Tree_Store.Key (Current_Root) < N;
               Child := Get_Child (Is_Right, Current_Root);
            end if;

            exit when Found or else Child = Node_Tree.Empty_Node;

            --  Traverse the tree: the Current_Root is set to one of its
            --  children.
            Current_Root := Child;
         end loop;
         --  The List.Visited stack will not be empty.
         --  if Found is True, the Tre_Store contains an Actual_Node with the
         --  value N.  The Tree_Node on the top of the List.Visted
         --  stack references this Actual_Node.
         --  If Found is False, the Tree_Store does not contain an Actual_Node
         --  with the value N. The Tree_Node at the top of List.Visited stack
         --  will contain the Tree_Node which will be the Parent of a
         --  Tree_Node referencing an Actual_Node containing N if it were to
         --  be added into the Tree_Store.
      end if;
   end Find;

   procedure Skew (Root : in out Node_Tree.Tree_Node)
     with Global => (In_Out => Tree_Store);

   procedure Skew (Root : in out Node_Tree.Tree_Node)
   --  --# global in out Tree_Store;
   is
      Left_Child : constant Node_Tree.Tree_Node := Tree_Store.Left (Root);
   begin
      --  No action is performed if the levels of the root and left nodes
      --  are not equal.
      if Tree_Store.Present (Left_Child) and then
        Tree_Store.Level (Left_Child) = Tree_Store.Level (Root)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Tree_Store.Set_Left
           (N      => Root,
            Branch => Tree_Store.Right (Left_Child));
         Tree_Store.Set_Right
           (N      => Left_Child,
            Branch => Root);
         --  The root now becomes the left child.
         Root := Left_Child;
      end if;
   end Skew;

   procedure Split (Root : in out Node_Tree.Tree_Node)
     with Global => (In_Out => Tree_Store);

   procedure Split (Root : in out Node_Tree.Tree_Node)
    --  --# global in out Tree_Store;
   is
      Right_Child : constant Node_Tree.Tree_Node :=
        Tree_Store.Right (Root);
      Right_Right_Child : Node_Tree.Tree_Node :=
        (if Tree_Store.Present (Right_Child) then
              Tree_Store.Right (Right_Child)
         else
            Right_Child);
   begin
      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Tree_Store.Present (Right_Right_Child) and then
        Tree_Store.Level (Right_Right_Child) = Tree_Store.Level (Root)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the ruight child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.
         Tree_Store.Set_Right
           (N      => Root,
            Branch => Tree_Store.Left (Right_Child));
         Tree_Store.Set_Left
           (N      => Right_Child,
            Branch => Root);
         --  The root now becomes the right child.
         Root := Right_Child;
         --  Increment the level of the new root.
         Tree_Store.Set_Level (Root,
                               Tree_Store.Level (Root) + 1);
      end if;
   end Split;

   procedure Rebalance_Tree (List : in out Node_List)
     with Global => (In_Out => Tree_Store)
     --  --# global in out Tree_Store;
   is
      Current_Root : Node_Tree.Tree_Node := List.Root;
      Top_Node     : Node_Tree.Tree_Node;
      --  The parent of the Current_Root
      Parent        : Node_Tree.Tree_Node;
      Is_Right     : Boolean;
   begin
      --  Rebalance the tree by working back up through the visited
      --  node indices on the List.Visited stack.
      for Stack_Top in reverse Natural range 1 .. List.Visited.Count loop
         --  Make the Current_Root equal to the Tree_Node at the top of
         --  the stack and the Current_Node equal to the node selected
         --  by the Current_Root.
         List.Visited.Pop (Top_Node);
         Current_Root := Top_Node;

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Current_Root
            --  As the Root has a parent, determine
            --  whether the Current_Root is a left or right child of
            --  its parent.
            Parent := List.Visited.Top;
            --  This boolean expression determines which branch of
            --  the parent has the the Current_Root as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Root.
            Is_Right :=
              Tree_Store.Right (Parent) = Top_Node;
         end if;

         --  Perform the Anderson Tree Skew and Split operations on
         --  the Current_Root.  The node which is the Current_Root
         --  may be changed by Skew and Split and so the Index contained
         --  in Current_Root may change.
         Skew (Current_Root);
         Split (Current_Root);

         --  Update the parent node to point to its new child.
         if Current_Root /= Top_Node and then Stack_Top > 1 then
            --  The value of the Current_Root
            --  may have changed and the stack has the
            --  parent of the Current_Root at the top of the
            --  visited stack.
            --  The branch that has the Current_Root as its child
            --  has to be patched up to contain the new value of
            --  Current_Root as its child.
            --  As the value of the Current_Root may have changed.
            Set_Branch
              (Is_Right => Is_Right,
               Node     => Parent,
               New_Node => Current_Root);
         end if;
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      List.Root := Current_Root;
   end Rebalance_Tree;

   -------------------
   -- Building_List --
   -------------------

   function Building_List return Boolean
     with Refined_Global => In_Progress
   is
   begin
      return In_Progress;
   end Building_List;
   pragma Inline (Building_List);

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List (N_List : Node_List) return Boolean is
   begin
      return N_List.Root = Node_Tree.Empty_Node;
   end Empty_List;

   --------------
   -- New_List --
   --------------

   procedure New_List (List : out Node_List)
     with Refined_Global => (Output => In_Progress)
     --  --# global out In_Progress;
   is
   begin
      In_Progress := True;
      List.Root := Node_Tree.Empty_Node;
      List.Visited.New_Stack;
   end New_List;

   --------------
   -- Insert_ --
   --------------

   procedure Insert (N : Types.Node_Id;
                     List : in out Node_List;
                     Inserted : out Boolean)
     with Refined_Global => (In_Out => Tree_Store)
     --  --# global in out Tree_Store;
   is
      Key_Found : Boolean;
      Is_Right      : Boolean;
      New_Node      : Node_Tree.Tree_Node;
      Current_Node  : Node_Tree.Tree_Node;
       --  A Child of the Current Node.
      Child         : Node_Tree.Tree_Node;
   begin
      if not Tree_Store.Present (List.Root) then
         --  First node of list - Enter a new node with level 1 into the store
         Inserted := True;
         Tree_Store.Add_Node
           (N => New_Node,
            V => Node_Tree.Element_Type'(Key => N));
         --  The new node is the root of the new list
         List.Root := New_Node;
      else
         --  Make sure that the list does not already include the key.
         Find (N, List, Key_Found);
         if Key_Found then
            --  The Node_Id, N, is already in the list, do not add it again.
            Inserted := False;
         elsif List.Visited.Is_Empty then
         --  The stack is empty and so the list is empty, there cannot be
         --  a duplicate.
         --  First node of list - Enter a new node with level 1 into the store
            Inserted := True;
            Tree_Store.Add_Node
              (N => New_Node,
               V => Node_Tree.Element_Type'(Key => N));
            --  The new node is the root of the new list
            List.Root := New_Node;
         else
            Inserted := True;
            Current_Node := List.Visited.Top;
            --  A right branch if the value of N is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Tree_Store.Key (Current_Node) < N;

            --  Add a new child node to extend the tree
            Tree_Store.Add_Node
              (N => Child,
               V => Node_Tree.Element_Type'(Key => N));
            Set_Branch (Is_Right => Is_Right,
                        Node     => Current_Node,
                        New_Node => Child);
            -- Now rebalance the tree
           Rebalance_Tree (List);
         end if;
      end if;
   end Insert;

   ---------------
   -- Save_List --
   ---------------

   procedure Save_List (List : in out Node_List)
   is
   begin
      In_Progress := False;
   end Save_List;

   -----------------
   --  Is_Present --
   -----------------

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
   is
   begin
      return False;
   end Is_Present;

   procedure Trace_To_Left_Leaf (E : in out Enumerator)
     with Global => (Input => Tree_Store),
          Pre    => not E.Visited.Is_Empty;

   procedure Trace_To_Left_Leaf (E : in out Enumerator)
     --  --# global in Tree_Store;
     --  --# pre not E.Visited.Is_Empty;
   is
      Current_Node : Node_Tree.Tree_Node :=
        Tree_Store.Left (E.Visited.Top);
   begin
--        Put_Line ("Trace_To_Left_Leaf - initial top: " &
--                    Types.Node_Id'Image (Tree_Store.Value (E.Visited.Top)));
      while Tree_Store.Present (Current_Node) loop
            E.Visited.Push (Current_Node);
            Current_Node := Tree_Store.Left (Current_Node);
      end loop;
--        Put_Line ("Trace_To_Left_Leaf - traced top: " &
--                    Types.Node_Id'Image (Tree_Store.Value (E.Visited.Top)));
   end Trace_To_Left_Leaf;

   -----------------
   --  Tree_Depth --
   -----------------

  function Tree_Depth (N_List : Node_List) return Natural
   --  -- global in Tree_Store;
     with Refined_Global => (Input => Tree_Store)
   is
      Result : Natural;
   begin
      if Empty_List (N_List) then
         Result := 0;
      else
         Result := Tree_Store.Level (N_List.Root);
      end if;
      return Result;
   end Tree_Depth;

   function New_Enumerator (N_List : Node_List) return Enumerator is
      Result : Enumerator;
   begin
      Result.Root := N_List;
      Result.Visited.New_Stack;
      Result.Visited.Push (N_List.Root);
--        Put_line ("New_Enumerator before Trace_Left");
--        Print_Stack (Result.Visited);
      Trace_To_Left_Leaf (Result);
--        Put_line ("New_Enumerator after Trace_Left");
--        Print_Stack (Result.Visited);
     return Result;
   end New_Enumerator;

   procedure Next (E : in out Enumerator; Next_Value : out Types.Node_Id)
   is
      Current_Node : Node_Tree.Tree_Node;
   begin
      if not E.Visited.Is_Empty then
--           Put_Line ("Before Pop " & Types.Node_Id'Image
--                     (Tree_Store.Value (E.Visited.Top)));
         E.Visited.Pop (Current_Node);
--           Put_Line ("After Pop " & Types.Node_Id'Image
--                     (Tree_Store.Value (E.Visited.Top)));
         Next_Value := Tree_Store.Key (Current_Node);
         Current_Node := Tree_Store.Right (Current_Node);
         if Current_Node /= Node_Tree.Empty_Node then
            E.Visited.Push (Current_Node);
            Trace_To_Left_Leaf (E);
         else
--              Put_Line ("No right branch");
            null;
         end if;
      else
         Next_Value := Types.Empty;
      end if;
   end Next;

   end SPARK_Classic.Symbols.Nodes;

