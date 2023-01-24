package body SPARK_Classic.Atrees is

   --  Basic Predicate
   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree (Tree : A_Tree) return Boolean
   --# return Tree.Root = Trees.Empty_Node;
   is
   begin
      return Tree.Root = Trees.Empty_Node;
   end Empty_Tree;

   function Populated (Tree : A_Tree; Tree_Store : Trees.Tree_Type)
                       return Boolean
   --# return (Trees.In_Tree (Tree_Store, Tree.Root) and
   --#        Tree.Count > 0);
   is
   begin
      return Trees.In_Tree (Tree_Store, Tree.Root) and
        Tree.Count > 0;
   end Populated;

   --  Proof helper subprograms

   --# function Is_Found (Found : Boolean) return Boolean;
   --# return Found;

   --# function Tree_Count (Tree : A_Tree) return Natural;
   --# return C => C = Tree.Count and
   --#        Tree_Count (Tree) = Count (Tree);

   procedure Push_In_Tree_Node (S : in out Bounded_Stacks.Stack;
                                Tree_Store : Trees.Tree_Type;
                                Node : Tree_Node)
   --# pre Trees.In_Tree (Tree_Store, Node);
   --# post not Bounded_Stacks.Is_Empty (S);
   is
   begin
      --# accept F, 30, Tree_Store, "Tree_Store only used in proof context.";

      --# accept W, 444, "A Stack_Size of 32 allows the traversal of ",
      --#                "balanced tree with more distinct nodes ",
      --#                "than can be handled by the gnat front-end";
      --# assume Bounded_Stacks.Count (S) < Bounded_Stacks.Stack_Size;

      Bounded_Stacks.Push (S, Node);
   end Push_In_Tree_Node;
   pragma Inline (Push_In_Tree_Node);

   procedure Pop_In_Tree_Node (S : in out Bounded_Stacks.Stack;
                               Tree_Store : Trees.Tree_Type;
                               Node : out Tree_Node)
   --# pre not Bounded_Stacks.Is_Empty (S);
   --# post Trees.In_Tree (Tree_Store, Node) and
   --#      Bounded_Stacks.Count (S) = Bounded_Stacks.Count (S~) - 1;
   is
   begin
      --# accept F, 30, Tree_Store, "Tree_Store only used in proof context.";

      Bounded_Stacks.Pop (S, Node);

      --# accept W, 444, "The exclusive use of Push_In_Tree ensures all ",
      --#                "pushed nodes are In_Tree, so, ",
      --#                "all nodes popped by Pop_In_Tree_Node will also be.";
      --# assume Trees.In_Tree (Tree_Store, Node);
   end Pop_In_Tree_Node;
   pragma Inline (Pop_In_Tree_Node);

   function Top_In_Tree_Node (S : Bounded_Stacks.Stack;
                              Tree_Store : Trees.Tree_Type) return Tree_Node
   --# pre not Bounded_Stacks.Is_Empty (S);
   --# return N => Trees.In_Tree (Tree_Store, N);
   is
      Result : Tree_Node;
   begin
      --# accept F, 30, Tree_Store, "Tree_Store only used in proof context.";

      Result := Bounded_Stacks.Top (S);

      --# accept W, 444, "The exclusive use of Push_In_Tree ensures all ",
      --#                "pushed nodes are In_Tree, so, ",
      --#                "all nodes in the stack are In_Tree";
      --# assume Trees.In_Tree (Tree_Store, Result);

      --# accept F, 50, Tree_Store, "Tree_Store only used in proof context.";

      return Result;
   end Top_In_Tree_Node;
   pragma Inline (Top_In_Tree_Node);

   --  Local subprograms
   procedure New_Node (Key        : Key_Type;
                       Tree_Store : in out Trees.Tree_Type;
                       The_Node   : out Tree_Node)
   is
   begin
      Trees.Add_Node (Tree_Store, The_Node, Key);
   end New_Node;

   function Get_Child (Is_Right : Boolean;
                       Node : Tree_Node;
                       Tree_Store : Trees.Tree_Type)
                       return Tree_Node
   --# pre Trees.In_Tree (Tree_Store, Node);
   --# return C => ((C /= Trees.Empty_Node) -> Trees.In_Tree (Tree_Store, C));
   is
      Result : Tree_Node;
   begin
      if Is_Right then
         Result := Trees.Right (Tree_Store, Node);
      else
         Result := Trees.Left (Tree_Store, Node);
      end if;
      return Result;
   end Get_Child;
   pragma Inline (Get_Child);

   procedure Set_Branch (Is_Right   : Boolean;
                         Node       : Tree_Node;
                         Set_Node   : Tree_Node;
                         Tree_Store : in out Trees.Tree_Type)
   --# post Trees.Persists (Tree_Store~, Tree_Store);
   is
   begin
      if Is_Right then
         Trees.Set_Right
           (T      => Tree_Store,
            N      => Node,
            Branch => Set_Node);
      else
         Trees.Set_Left
           (T      => Tree_Store,
            N      => Node,
            Branch => Set_Node);
      end if;
   end Set_Branch;
   pragma Inline (Set_Branch);

   procedure Find (Tree       : A_Tree;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Tree_Store : Trees.Tree_Type;
                   Visited    : out Bounded_Stacks.Stack)
   --# pre Populated (Tree, Tree_Store);
   --# post Found -> (Is_Found (Found) and
   --#                not Bounded_Stacks.Is_Empty (Visited));
     --  The Tree is not empty (Populated) so the given Key may be present.
     --  If found is true, the top of the Tree.Visited stack is the Tree_Node
     --  that references the The Actual_Node in the Tree_Store which
     --  contains the Key.
   is

      Current_Node  : Tree_Node;

      procedure Find_Proof (Tree         : A_Tree;
                            Key          : Key_Type;
                            Found        : out Boolean;
                            Tree_Store   : Trees.Tree_Type;
                            Visited      : out Bounded_Stacks.Stack;
                            Current_Node : out Tree_Node)
     --# pre Populated (Tree, Tree_Store);
     --# post Is_Found (Found) = (Trees.Key (Tree_Store, Current_Node) = Key)
     --#       and (Found -> not Bounded_Stacks.Is_Empty (Visited));
      is
         Current_Key   : Key_Type;

         --  A Child of the current node.
         Child         : Tree_Node;
         --  Direction: Left = False, Right = True
         Is_Right      : Boolean;

      begin
         --  Assume that a match for the Key, has not been found.
         --  Found := False;
         Child := Trees.Empty_Node;
         --  Clear the visited stack - the Tree is being searced from its root.
         Bounded_Stacks.Clear (Visited);

         --  If the Tree.Root is not present, the Tree is empty
         --  and the given Key will not be found.
         --  There is nothing more to be done.

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
            Push_In_Tree_Node (Visited, Tree_Store, Current_Node);
            Current_Key := Trees.Key (Tree_Store, Current_Node);
            Found := Current_Key = Key;
            if not Found then
               --  Take the right branch if the Key value is greater
               --  than the Current_Node Key, otherwise take the left branch.
               Is_Right := Trees.Key (Tree_Store, Current_Node) < Key;
               Child := Get_Child (Is_Right, Current_Node, Tree_Store);
            end if;

            exit when Found or else not Trees.In_Tree (Tree_Store, Child);

            --  Traverse the tree: the Current_Node is set to one of its
            --  children.
            Current_Node := Child;
            --# assert not (Found or (not Trees.In_Tree (Tree_Store, Current_Node)));
         end loop;
         --# check Found -> (Trees.Key (Tree_Store, Current_Node) = Key);
         --  The Tree.Visited stack will not be empty.
         --  if Found is True, the Tree_Store contains an Actual_Node with the
         --  matching Key.  The Tree_Node on the top of the Tree.Visted
         --  stack references this Actual_Node.
         --  If Found is False, the Tree_Store does not contain an Actual_Node
         --  with a matching Key. The Tree_Node at the top of
         --  Tree.Visited stack will contain the Tree_Node which will be the
         --  Parent of a Tree_Node referencing an Actual_Node containing
         --  the Key if it were to be added into the Tree.
      end Find_Proof;

   begin
      --# accept F, 10, Current_Node, "Current_Node exported only for proof" &
      --#        F, 33, Current_Node, "Current_Node exported only for proof";
      Find_Proof (Tree         => Tree,
                  Key          => Key,
                  Found        => Found,
                  Tree_Store   => Tree_Store,
                  Visited      => Visited,
                  Current_Node => Current_Node);
   end Find;

   procedure Skew (Root       : in out Tree_Node;
                   Tree_Store : in out Trees.Tree_Type)
   is
      Left_Child : Trees.Tree_Node;
   begin
      Left_Child := Trees.Left (Tree_Store, Root);
      --  No action is performed if the levels of the root and left nodes
      --  are not equal.
      if Trees.In_Tree (Tree_Store, Left_Child) and then
        Trees.Level (Tree_Store, Left_Child) = Trees.Level (Tree_Store, Root)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Trees.Set_Left
           (T      => Tree_Store,
            N      => Root,
            Branch => Trees.Right (Tree_Store, Left_Child));
         Trees.Set_Right
           (T      => Tree_Store,
            N      => Left_Child,
            Branch => Root);
         --  The root now becomes the left child.
         Root := Left_Child;
      end if;
   end Skew;

   procedure Split (Root       : in out Tree_Node;
                    Tree_Store : in out Trees.Tree_Type)
   is
      Right_Child       : Tree_Node;
      Right_Right_Child : Tree_Node;
   begin
      Right_Child  := Trees.Right (Tree_Store, Root);
      if Trees.In_Tree (Tree_Store, Right_Child) then
         Right_Right_Child := Trees.Right (Tree_Store, Right_Child);
      else
         Right_Right_Child := Trees.Empty_Node;
      end if;

      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Trees.In_Tree (Tree_Store, Right_Right_Child) and then
        Trees.Level (Tree_Store, Right_Right_Child) = Trees.Level (Tree_Store, Root)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the ruight child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.
         Trees.Set_Right
           (T      => Tree_Store,
            N      => Root,
            Branch => Trees.Left (Tree_Store, Right_Child));
         Trees.Set_Left
           (T      => Tree_Store,
            N      => Right_Child,
            Branch => Root);
         --  The root now becomes the right child.
         Root := Right_Child;
         --  Increment the level of the new root.
         Trees.Set_Level (Tree_Store,
                          Root,
                          Trees.Level (Tree_Store, Root) + 1);
      end if;
   end Split;

   procedure Rebalance_Tree (Tree       : in out A_Tree;
                             Tree_Store : in out Trees.Tree_Type;
                             Visited    : in out Bounded_Stacks.Stack)
   --# pre Populated (Tree, Tree_Store);
   --# post Populated (Tree, Tree_Store) and
   --#      Persists (Tree~, Tree, Tree_Store~, Tree_Store);
   is
      Current_Node : Tree_Node;
      Top_Node     : Tree_Node;
      --  The parent of the Current_Node
      Parent       : Tree_Node;
      Is_Right     : Boolean;
      Stack_Count  : Natural;
   begin
      --  The following two initalizing statements avoid
      --  flow errors using SPARK 2005 Examiner.
      Is_Right := False;
      Parent := Trees.Empty_Node;

      Current_Node := Tree.Root;
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Tree.Visited stack.
      Stack_Count := Bounded_Stacks.Count (Visited);
      for Stack_Top in reverse Natural range 1 .. Stack_Count
      loop
         --# assert Stack_Top > 0 and Stack_Top <= Stack_Count and
         --#        Stack_Top = Bounded_Stacks.Count (Visited) and
         --#        not Bounded_Stacks.Is_Empty (Visited) and
         --#        Persists (Tree~, Tree, Tree_Store~, Tree_Store);
         --  Make the Current_Node equal to the Tree_Node at the top of
         --  the stack.
         Pop_In_Tree_Node (Visited, Tree_Store, Top_Node);
         Current_Node := Top_Node;

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Current_Node
            --  As the Current_Node has a parent, determine
            --  whether the Current_Node is a left or right child of
            --  its parent.
            Parent := Top_In_Tree_Node (Visited,  Tree_Store);
            --  This boolean expression determines which branch of
            --  the parent has the the Current_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Node.
            Is_Right :=
              Trees.Right (Tree_Store, Parent) = Top_Node;
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
               Set_Node   => Current_Node,
               Tree_Store => Tree_Store);
         end if;
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      Tree.Root := Current_Node;
   end Rebalance_Tree;

   procedure Trace_To_Left_Leaf (E : in out Enumerator;
                                 Tree_Store : Trees.Tree_Type)
   is
      Current_Node : Tree_Node;
   begin
      Current_Node :=
        Trees.Left (Tree_Store, Top_In_Tree_Node (E.Visited, Tree_Store));
      while Trees.In_Tree (Tree_Store, Current_Node) loop
            Push_In_Tree_Node (E.Visited, Tree_Store, Current_Node);
            Current_Node := Trees.Left (Tree_Store, Current_Node);
      end loop;
   end Trace_To_Left_Leaf;

   procedure Init_Enumerator (Tree       : A_Tree;
                              Tree_Store : Trees.Tree_Type;
                              Enum       : out Enumerator)
   --# pre Trees.In_Tree (Tree_Store, Tree.Root);
   is
   begin
      Enum.Root := Tree;
      Bounded_Stacks.New_Stack (Enum.Visited);
      Push_In_Tree_Node (Enum.Visited, Tree_Store, Tree.Root);
      Trace_To_Left_Leaf (Enum, Tree_Store);
   end Init_Enumerator;

  --------------
   -- New_Tree --
   --------------

   procedure New_Tree (Tree       : out A_Tree)
   is
   begin
      Tree.Root := Trees.Empty_Node;
      Tree.Count := 0;
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
      Visited       : Bounded_Stacks.Stack;
      Key_Found     : Boolean;
      Is_Right      : Boolean;
      Insert_Node   : Trees.Tree_Node;
      Current_Node  : Trees.Tree_Node;
       --  A Child of the Current Node.
      Child         : Trees.Tree_Node;
   begin
      --# check Tree_Count (Tree) = Count (Tree);
      if not Populated (Tree, Tree_Store) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         Tree.Count := 1;
         Trees.Add_Node
           (T       => Tree_Store,
            N       => Insert_Node,
            The_Key => Key);
         --  The new node is the root of the new tree
         Tree.Root := Insert_Node;
      else
         --  Make sure that the tree does not already include the key.
         Find (Tree, Key, Key_Found, Tree_Store, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
         elsif Bounded_Stacks.Is_Empty (Visited) then
         --  The stack is empty and so the tree is empty, there cannot be
         --  a duplicate key.
         --  First node of tree - Enter a new node with level 1 into the store
            Inserted := True;
            --# accept W, 444, "The number of nodes is bounded by ",
            --#                "the number of possible nodes.";
            --# assume Tree.Count <= Integer (Trees.Tree_Node'Last);
            --# end accept;
            Tree.Count := Tree.Count + 1;
            Trees.Add_Node
              (T       => Tree_Store,
               N       => Insert_Node,
               The_Key => Key);
            --  The new node is the root of the new tree
            Tree.Root := Insert_Node;
         else
            Inserted := True;
            --# accept W, 444, "The number of nodes is bounded by ",
            --#                "the number of possible nodes.";
            --# assume Tree.Count <= Integer (Trees.Tree_Node'Last);
            --# end accept;
            Tree.Count := Tree.Count + 1;
            Current_Node := Top_In_Tree_Node (Visited, Tree_Store);
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Trees.Key (Tree_Store, Current_Node) < Key;

            --  Add a new child node to extend the tree
            Trees.Add_Node
              (T       => Tree_Store,
               N       => Child,
               The_Key => Key);
            --# check Tree_Count (Tree) = Count (Tree);
             Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        Set_Node   => Child,
                        Tree_Store => Tree_Store);
            --# check Trees.in_Tree (Tree_Store, Tree.Root);
            --# check Populated (Tree, Tree_Store);
            -- Now rebalance the tree
            --# accept F, 10, Visited, "Visited must be an in out paramter ",
            --#                        "as it is updated by Rebalance_Tree",
            --#                        " but its final value is unrequired";
            Rebalance_Tree (Tree, Tree_Store, Visited);
         end if;
      end if;
      --# check Tree_Count (Tree) = Tree.Count;
      --# accept W, 444, "The Key has been inserted into the Tree";
      --#  assume Is_Present (Tree, Key, Tree_Store);
      --# end accept;
   end Insert;

   -----------------------
   -- Insert_With_Value --
   -----------------------

   procedure Insert_With_Value
     (Tree          : in out A_Tree;
      Key           : Key_Type;
      Value         : Value_Type;
      Tree_Store    : in out Trees.Tree_Type;
      Inserted      : out Boolean;
      Value_At_Node : out Value_Type)
   is
      Visited       : Bounded_Stacks.Stack;
      Key_Found     : Boolean;
      Is_Right      : Boolean;
      Insert_Node   : Trees.Tree_Node;
      Current_Node  : Trees.Tree_Node;
       --  A Child of the Current Node.
      Child         : Trees.Tree_Node;
   begin
       --# check Tree_Count (Tree) = Count (Tree);
     if not Populated (Tree, Tree_Store) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         Tree.Count := 1;
         Trees.Add_Node
           (T       => Tree_Store,
            N       => Insert_Node,
            The_Key => Key);
         Value_At_Node := Value;
         Trees.Set_Value (Tree_Store, Insert_Node, Value_At_Node);
         --  The new node is the root of the new tree
         Tree.Root := Insert_Node;
      else
         --  Make sure that the tree does not already include the key.
         Find (Tree, Key, Key_Found, Tree_Store, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
            --  The node with the key is on the top of the visited stack.
            --  Get its value.
            Value_At_Node :=
              Trees.Value (Tree_Store, Top_In_Tree_Node (Visited, Tree_Store));
         elsif Bounded_Stacks.Is_Empty (Visited) then
         --  The stack is empty and so the tree is empty, there cannot be
         --  a duplicate key.
         --  First node of tree - Enter a new node with level 1 into the store
            Inserted := True;
            --# accept W, 444, "The number of nodes is bounded by ",
            --#                "the number of possible nodes.";
            --# assume Tree.Count <= Integer (Trees.Tree_Node'Last);
            --# end accept;
            Tree.Count := Tree.Count + 1;
            Trees.Add_Node
              (T       => Tree_Store,
               N       => Insert_Node,
               The_Key => Key);
            Value_At_Node := Value;
            Trees.Set_Value (Tree_Store, Insert_Node, Value_At_Node);
            --  The new node is the root of the new tree
            Tree.Root := Insert_Node;
         else
            Inserted := True;
            --# accept W, 444, "The number of nodes is bounded by ",
            --#                "the number of possible nodes.";
            --# assume Tree.Count <= Integer (Trees.Tree_Node'Last);
            --# end accept;
            Tree.Count := Tree.Count + 1;
            Current_Node := Top_In_Tree_Node (Visited, Tree_Store);
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Trees.Key (Tree_Store, Current_Node) < Key;

            --  Add a new child node to extend the tree
            Trees.Add_Node
              (T       => Tree_Store,
               N       => Child,
               The_Key => Key);
            --# check Tree_Count (Tree) = Count (Tree);
            Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        Set_Node   => Child,
                        Tree_Store => Tree_Store);
           Value_At_Node := Value;
            Trees.Set_Value (Tree_Store, Child, Value_At_Node);
            --# check Trees.in_Tree (Tree_Store, Tree.Root);
            --# check Populated (Tree, Tree_Store);
            -- Now rebalance the tree
            --# accept F, 10, Visited, "Visited must be an in out paramter ",
            --#                        "as it is updated by Rebalance_Tree",
            --#                        " but its final value is unrequired";
            Rebalance_Tree (Tree, Tree_Store, Visited);
         end if;
      end if;
      --# check Tree_Count (Tree) = Tree.Count;
      --# accept W, 444, "The Key has been inserted into the Tree";
      --#  assume Is_Present (Tree, Key, Tree_Store);
      --# end accept;
   end Insert_With_Value;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree       : in out A_Tree;
                    Tree_Store : in out Trees.Tree_Type)
   is
   begin
      Trees.Clear (Tree_Store, Tree.Root);
      --  Tree := Null_A_Tree;
      --  SPARK Examiner 2005 Simplifier does not infer that
      --  Tree := Null_A_Tree implies Tree.Root = Trees.Empty_Node;
      Tree.Root := Trees.Empty_Node;
      Tree.Count := 0;
   end Clear;

   ---------------
   -- Next_Node --
   ---------------

   procedure Next_Node (E    : in out Enumerator; Tree_Store : Trees.Tree_Type;
                        Node : out Tree_Node) is
      Right_Child : Trees.Tree_Node;
   begin
      if not Bounded_Stacks.Is_Empty (E.Visited) then
         Pop_In_Tree_Node (E.Visited, Tree_Store, Node);
         Right_Child := Trees.Right (Tree_Store, Node);
         if  Trees.In_Tree (Tree_Store, Right_Child) then
            Push_In_Tree_Node (E.Visited, Tree_Store, Right_Child);
            Trace_To_Left_Leaf (E, Tree_Store);
         end if;
      else
         Node := Trees.Empty_Node;
      end if;
   end Next_Node;
   pragma Inline (Next_Node);

    -------------
   -- Is_Equal --
   --------------

  function Is_Equal (Tree_1       : A_Tree;
                     Tree_2       : A_Tree;
                     Tree_Store_1 : Trees.Tree_Type;
                     Tree_Store_2 : Trees.Tree_Type) return Boolean
   is
      Enum_1    : Enumerator;
      Enum_2    : Enumerator;
      Current_1 : Tree_Node;
      Current_2 : Tree_Node;
      Present_1 : Boolean;
      Present_2 : Boolean;
      Both_Present : Boolean;
      Equal     : Boolean;
   begin
      Equal := Tree_1.Count = Tree_2.Count;
      if Equal then
         Init_Enumerator (Tree_1, Tree_Store_1, Enum_1);
         Init_Enumerator (Tree_2, Tree_Store_2, Enum_2);
         loop
            Next_Node (Enum_1, Tree_Store_1, Current_1);
            Next_Node (Enum_2, Tree_Store_2, Current_2);
            Present_1 := Trees.In_Tree (Tree_Store_1, Current_1);
            Present_2 := Trees.In_Tree (Tree_Store_2, Current_2);
            Both_Present := Present_1 and Present_2;
            if Both_Present then
               Equal := Trees.Key (Tree_Store_1, Current_1) =
                 Trees.Key (Tree_Store_2, Current_2);
            else
               Equal := False;
            end if;
            exit when not Equal or else not Both_Present;
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
   --# pre Populated (Tree, Tree_Store);
   --# return R => (R -> Is_Found (R));
   is
      Visited : Bounded_Stacks.Stack;
      Found   : Boolean;
   begin
      --# accept F, 10, Visited, "Visited stack is used by Find" &
      --#        F, 33, Visited, "Final value of Visited is not required";
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
      if Empty_Tree (Tree) then
         Result := 0;
      else
         Result := Trees.Level (Tree_Store, Tree.Root);
      end if;
      return Result;
   end Tree_Depth;

   -----------
   -- Count --
   -----------

   function Count (Tree : A_Tree) return Natural
   --# return Tree_Count (Tree);
   is
   begin
      return Tree.count;
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
      Bounded_Stacks.New_Stack (Result.Visited);
      Push_In_Tree_Node (Result.Visited, Tree_Store, Tree.Root);
      Trace_To_Left_Leaf (Result, Tree_Store);
      return Result;
   end New_Enumerator;

end SPARK_Classic.Atrees;
