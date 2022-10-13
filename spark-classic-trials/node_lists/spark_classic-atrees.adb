with SPARK_Classic.Simple_Stack;
package body SPARK_Classic.Atrees is

   --  Basic Predicate
   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree (Tree       : A_Tree;
                        Tree_Store : Tree_Type)
                        return Boolean
   is
   begin
      return not Tree_Store.Present (Tree.Root);
   end Empty_Tree;

   --  Local subprograms
   procedure New_Node (Key        : Key_Type;
                       Tree_Store : in out Tree_Type;
                       The_Node   : out Tree_Node);

   procedure New_Node (Key        : Key_Type;
                       Tree_Store : in out Tree_Type;
                       The_Node   : out Tree_Node)
   is
   begin
      Tree_Store.Add_Node (The_Node, Key);
   end New_Node;

   function Get_Child (Is_Right : Boolean;
                       Node : Tree_Node;
                       Tree_Store : Tree_Type)
                       return Tree_Node;

   function Get_Child (Is_Right : Boolean;
                       Node : Tree_Node;
                       Tree_Store : Tree_Type)
                       return Tree_Node
   is
      Result : Tree_Node;
   begin
      if Is_Right then
         Result := Tree_Store.Right (Node);
      else
         Result := Tree_Store.Left (Node);
      end if;
      return Result;
   end Get_Child;
   pragma Inline (Get_Child);

   procedure Set_Branch (Is_Right   : Boolean;
                         Node       : Tree_Node;
                         New_Node   : Tree_Node;
                         Tree_Store : in out Tree_Type);

   procedure Set_Branch (Is_Right   : Boolean;
                         Node       : Tree_Node;
                         New_Node   : Tree_Node;
                         Tree_Store : in out Tree_Type)
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

   procedure Find (Tree       : A_Tree;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Tree_Store : Trees.Tree_Type;
                   Visited    : out Stacks.Stack);

   procedure Find (Tree       : A_Tree;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Tree_Store : Trees.Tree_Type;
                   Visited    : out Stacks.Stack)
       --  -- # global in Tree_Store
     --  If found is true, the top of the Tree.Visited stack is the Tree_Node
     --  that references the The Actual_Node in the Tree_Store which
     --  contains the Key.
   is
      Current_Node  : Tree_Node;
      Current_Key   : Key_Type;

      --  A Child of the current node.
      Child         : Tree_Node;
      --  Direction: Left = False, Right = True
      Is_Right      : Boolean;
   begin
      --  Assume that a match for the Key, has not been found.
      Found := False;
      --  Clear the visited stack - the Tree is being searced from its root.
      Visited.Clear;

      --  If the Tree.Root is not present, the Tree is empty
      --  and the given Key will not be found.
      --  There is nothing more to be done.
      if Tree_Store.Present (Tree.Root) then
         --  The Tree is not empty the given Key may be present.

         --  The Current_Node is initially set to the root of the tree.
         Current_Node := Tree.Root;
         --  Search the binary tree to find a matching Key or, if it is
         --  not found, locate an appropriate leaf to place the Key.
         --  If Found the node with the matching Key is on the top of the
         --  Tree.Visited stack.
         --  If not Found the top of the Visited stack contains the leaf node
         --  appropriate for the insertion of the key into one of its child
         --  branches.
         loop
            --  A record of nodes visited is held in the Visited stack.
            pragma Assume (Visited.Count < Stack_Size);
            Visited.Push (Current_Node);
            Current_Key := Tree_Store.Key (Current_Node);
            if Current_Key = Key then
               Found := True;
            else
               --  Take the right branch if the Key value is greater
               --  than the Current_Node Key, otherwise take the left branch.
               Is_Right := Tree_Store.Key (Current_Node) < Key;
               Child := Get_Child (Is_Right, Current_Node, Tree_Store);
            end if;

            exit when Found or else Child = Trees.Empty_Node;

            --  Traverse the tree: the Current_Node is set to one of its
            --  children.
            Current_Node := Child;
         end loop;
         --  The Tree.Visited stack will not be empty.
         --  if Found is True, the Tre_Store contains an Actual_Node with the
         --  matching Key.  The Tree_Node on the top of the Tree.Visted
         --  stack references this Actual_Node.
         --  If Found is False, the Tree_Store does not contain an Actual_Node
         --  with a matching Key. The Tree_Node at the top of
         --  Tree.Visited stack will contain the Tree_Node which will be the
         --  Parent of a Tree_Node referencing an Actual_Node containing
         --  the Key if it were to be added into the Tree.
      end if;
   end Find;

   procedure Skew (Root       : in out Tree_Node;
                   Tree_Store : in out Tree_Type);

   procedure Skew (Root       : in out Tree_Node;
                   Tree_Store : in out Trees.Tree_Type)
   is
      Left_Child : constant Trees.Tree_Node := Tree_Store.Left (Root);
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

   procedure Split (Root       : in out Tree_Node;
                    Tree_Store : in out Tree_Type);

   procedure Split (Root       : in out Tree_Node;
                    Tree_Store : in out Tree_Type)
   is
      Right_Child       : constant Tree_Node := Tree_Store.Right (Root);
      Right_Right_Child : Tree_Node;
   begin
      if Tree_Store.Present (Right_Child) then
         Right_Right_Child := Tree_Store.Right (Right_Child);
      else
         Right_Right_Child := Trees.Empty_Node;
      end if;

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

   procedure Rebalance_Tree (Tree       : in out A_Tree;
                             Tree_Store : in out Tree_Type;
                             Visited    : in out Stacks.Stack);

   procedure Rebalance_Tree (Tree       : in out A_Tree;
                             Tree_Store : in out Tree_Type;
                             Visited    : in out Stacks.Stack)

   is
      Current_Node : Tree_Node := Tree.Root;
      Top_Node     : Tree_Node;
      --  The parent of the Current_Node
      Parent       : Tree_Node;
      Is_Right     : Boolean;
   begin
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Tree.Visited stack.
      for Stack_Top in reverse Natural range 1 .. Visited.Count loop
         --  Make the Current_Node equal to the Tree_Node at the top of
         --  the stack.
         Visited.Pop (Top_Node);
         Current_Node := Top_Node;

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Current_Node
            --  As the Current_Node has a parent, determine
            --  whether the Current_Node is a left or right child of
            --  its parent.
            Parent := Visited.Top;
            --  This boolean expression determines which branch of
            --  the parent has the the Current_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Node.
            Is_Right :=
              Tree_Store.Right (Parent) = Top_Node;
         end if;

         --  Perform the Anderson Tree Skew and Split operations on
         --  the Current_Node.  The Current_Node
         --  may be changed by Skew and Split.
         Skew (Current_Node, Tree_Store);
         Split (Current_Node, Tree_Store);

         --  Update the parent node to point to its new child.
         if Current_Node /= Top_Node and then Stack_Top > 1 then
            --  The value of the Current_Node
            --  may have changed and the stack has the
            --  parent of the Current_Node at the top of the
            --  visited stack.
            --  The branch that has the Current_Node as its child
            --  has to be patched up to contain the new value of
            --  Current_Node as its child.
            --  As the value of the Current_Node may have changed.
            Set_Branch
              (Is_Right   => Is_Right,
               Node       => Parent,
               New_Node   => Current_Node,
               Tree_Store => Tree_Store);
         end if;
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      Tree.Root := Current_Node;
   end Rebalance_Tree;

   procedure Trace_To_Left_Leaf (E : in out Enumerator;
                                 Tree_Store : Tree_Type);

   procedure Trace_To_Left_Leaf (E : in out Enumerator;
                                 Tree_Store : Tree_Type)
   is
      Current_Node : Tree_Node :=
        Tree_Store.Left (E.Visited.Top);
   begin
--        Put_Line ("Trace_To_Left_Leaf - initial top: " &
--                    Types.Node_Id'Image (Tree_Store.Value (E.Visited.Top)));
      while Tree_Store.Present (Current_Node) loop
            E.Visited.Push (Current_Node);
            Current_Node := Tree_Store.Left (Current_Node);
      end loop;
   end Trace_To_Left_Leaf;

   procedure Equal_To_Left_Leaf (Tree_Store_1     : Tree_Type;
                                 Tree_Store_2   : Tree_Type;
                                 Visited_1      : in out Stacks.Stack;
                                 Visited_2      : in out Stacks.Stack;
                                 Equal          : out Boolean);

   procedure Equal_To_Left_Leaf (Tree_Store_1   : Tree_Type;
                                 Tree_Store_2   : Tree_Type;
                                 Visited_1      : in out Stacks.Stack;
                                 Visited_2      : in out Stacks.Stack;
                                 Equal          : out Boolean)
   is
      Current_1 : Tree_Node := Visited_1.Top;
      Current_2 : Tree_Node := Visited_2.Top;
      Present_1 : Boolean;
   begin
      loop
         Equal := Tree_Store_1.Key (Current_1) = Tree_Store_2.Key (Current_2);
         Current_1 := Tree_Store_1.Left (Current_1);
         Current_2 := Tree_Store_2.Left (Current_2);
         Present_1 := Tree_Store_1.Present (Current_1);
         Equal := Equal and
           not (Present_1 xor Tree_Store_2.Present (Current_2));
         exit when not (Equal and Present_1);
         Visited_1.Push (Current_1);
         Visited_2.Push (Current_2);
      end loop;
   end Equal_To_Left_Leaf;

  --------------
   -- New_Tree --
   --------------

   procedure New_Tree
     (Tree       : out A_Tree;
      Tree_Store : Tree_Type)
   is
   begin
      Tree.Root := Trees.Empty_Node;
      Tree.Count := 0;
      pragma Assert (not Tree_Store.Present (Tree.Root));
   end New_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree       : in out A_Tree;
      Key        : Key_Type;
      Tree_Store : in out Trees.Tree_Type;
      Inserted   : out Boolean)
   is
      Visited       : Stacks.Stack;
      Key_Found     : Boolean;
      Is_Right      : Boolean;
      New_Node      : Trees.Tree_Node;
      Current_Node  : Trees.Tree_Node;
       --  A Child of the Current Node.
      Child         : Trees.Tree_Node;
   begin
      if not Tree_Store.Present (Tree.Root) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         Tree.Count := Tree.Count + 1;
         Tree_Store.Add_Node
           (N   => New_Node,
            Key => Key);
         --  The new node is the root of the new tree
         Tree.Root := New_Node;
         --  The Visited stack is empty.
         Visited.Clear;
      else
         --  Make sure that the tree does not already include the key.
         Find (Tree, Key, Key_Found, Tree_Store, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
         elsif Visited.Is_Empty then
         --  The stack is empty and so the tree is empty, there cannot be
         --  a duplicate key.
         --  First node of tree - Enter a new node with level 1 into the store
            Inserted := True;
            Tree.Count := Tree.Count + 1;
            Tree_Store.Add_Node
              (N   => New_Node,
               Key => Key);
            --  The new node is the root of the new tree
            Tree.Root := New_Node;
         else
            Inserted := True;
            Tree.Count := Tree.Count + 1;
            Current_Node := Visited.Top;
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Tree_Store.Key (Current_Node) < Key;

            --  Add a new child node to extend the tree
            Tree_Store.Add_Node
              (N   => Child,
               Key => Key);
            Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        New_Node   => Child,
                        Tree_Store => Tree_Store);
            -- Now rebalance the tree
           Rebalance_Tree (Tree, Tree_Store, Visited);
         end if;
      end if;
   end Insert;

    -------------
   -- Is_Equal --
   --------------

  function Is_Equal (Tree_1       : A_Tree;
                     Tree_2       : A_Tree;
                     Tree_Store_1 : Tree_Type;
                     Tree_Store_2 : Tree_Type) return Boolean
   is
      Stack_1   : Stacks.Stack;
      Stack_2   : Stacks.Stack;
      Current_1 : Tree_Node;
      Current_2 : Tree_Node;
      Present_1 : Boolean;
      Equal     : Boolean;
   begin
      Equal := Tree_1.Count = Tree_2.Count;
      if Equal and Tree_1.Count /= 0 then
         Stack_1.New_Stack;
         Stack_2.New_Stack;
         Stack_1.Push (Tree_1.Root);
         Stack_2.Push (Tree_2.Root);
         while Equal and not Stack_1.Is_Empty loop
            Equal_To_Left_Leaf
              (Tree_Store_1 => Tree_Store_1,
               Tree_Store_2 => Tree_Store_2,
               Visited_1    => Stack_1,
               Visited_2    => Stack_2,
               Equal        => Equal);
            if Equal then
               Stack_1.Pop (Current_1);
               Stack_2.Pop (Current_2);
               Current_1 := Tree_Store_1.Right (Current_1);
               Current_2 := Tree_Store_2.Right (Current_2);
               Present_1 := Tree_Store_1.Present (Current_1);
               Equal := not (Present_1 xor Tree_Store_2.Present (Current_2));
               if Equal and Present_1 then
                  Stack_1.Push (Current_1);
                  Stack_2.Push (Current_2);
                  Equal_To_Left_Leaf
                    (Tree_Store_1 => Tree_Store_1,
                     Tree_Store_2 => Tree_Store_2,
                     Visited_1    => Stack_1,
                     Visited_2    => Stack_2,
                     Equal        => Equal);
               end if;
            end if;
         end loop;
      end if;
      return Equal;
   end Is_Equal;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present
     (Tree       : A_Tree;
      Key        : Key_Type;
      Tree_Store : Trees.Tree_Type)
      return Boolean
   is
      Visited : Stacks.Stack;
      Found   : Boolean;
   begin
      Find (Tree, Key, Found, Tree_Store, Visited);
      return Found;
   end Is_Present;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth
     (Tree       : A_Tree;
      Tree_Store : Trees.Tree_Type)
      return Natural
   is
      Result : Natural;
   begin
      if Tree.Empty_Tree (Tree_Store) then
         Result := 0;
      else
         Result := Tree_Store.Level (Tree.Root);
      end if;
      return Result;
   end Tree_Depth;

   -----------
   -- Count --
   -----------

   function Count (Tree : A_Tree) return Natural is
   begin
      return Tree.Count;
   end Count;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator
     (Tree       : A_Tree;
      Tree_Store : Trees.Tree_Type)
      return Enumerator
   is
      Result : Enumerator;
   begin
      Result.Root := Tree;
      Result.Visited.New_Stack;
      Result.Visited.Push (Tree.Root);
      Trace_To_Left_Leaf (Result, Tree_Store);
      return Result;
   end New_Enumerator;

   ---------------
   -- Next_Node --
   ---------------

   procedure Next_Node (E    : in out Enumerator; Tree_Store : Trees.Tree_Type;
                        Node : out Trees.Tree_Node) is
      Right_Child : Trees.Tree_Node;
   begin
      if not E.Visited.Is_Empty then
         E.Visited.Pop (Node);
         Right_Child := Tree_Store.Right (Node);
         if  Tree_Store.Present (Right_Child) then
            E.Visited.Push (Right_Child);
            Trace_To_Left_Leaf (E, Tree_Store);
         end if;
      else
         Node := Trees.Empty_Node;
      end if;
   end Next_Node;

end SPARK_Classic.Atrees;
