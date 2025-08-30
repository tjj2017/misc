package body Atrees
with SPARK_Mode
is
   package Tree_Abs is new
     Tree_Abstraction
       (Tree_Node  => Atree_Node,
        Level_Type => Natural,
        Key_Type   => Key_Type,
        Value_Type => Value_Type,
        Null_Key   => Null_Key,
        Null_Value => Null_Value);

   --  Basic Predicate
   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree (ATree : A_Tree) return Boolean is
      (ATree.Root = Empty_Node);

   ---------------
   -- Populated --
   ---------------

   function Populated (ATree : A_Tree) return Boolean is
      (Tree_Abs.In_Tree (ATree.Root) and
           ATree.Count > 0);

   -----------
   -- Count --
   -----------

   function Count (Atree : A_Tree) return Natural is (Atree.Count);

   -------------------
   -- Node_In_Atree --
   -------------------

   function Node_In_Atree (Atree : A_Tree; N : Atree_Node) return Boolean is
     (N >= Atree.Root and then Atree.Count > 0
      and then Integer (N + Atree.Root) <= Integer (Atree.Root) + Atree.Count
      and then Tree_Abs.In_Tree (N));

   --  --  Proof helper subprograms
   --
   --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the Tree.
   function Top_In_Atree_Node (S : Bounded_Stack.Stack;
                               Tree : A_Tree) return Atree_Node
   with Pre  => not Bounded_Stack.Is_Empty (S),
        Post =>  Node_In_Atree (Tree, Top_In_Atree_Node'Result)
   is
      Result : Atree_Node;
   begin
      Result := Bounded_Stack.Top (S);
      pragma Assume (Node_In_Atree (Tree, Result),
                     "The exclusive use of Push_In_Tree ensures all " &
                     "pushed nodes are In_Tree, so, " &
                     "all nodes popped by Pop_In_Tree_Node will also be.");

      return Result;
      pragma Warnings (Off, "unused variable ""Tree""",
                      Reason => "Tree is only used in proof context");
   end Top_In_Atree_Node;
   pragma Warnings (On, "unused variable ""Tree""");
   pragma Inline (Top_In_Atree_Node);

   --  Ensures that each Node pushed on the stack is in the Tree.
   --  With a stack size of 32 a balanced tree would be enormous, it would
   --  have more nodes than the number actually available.
   procedure Push_In_Atree_Node (S : in out Bounded_Stack.Stack;
                                 Tree : A_Tree;
                                 Node : Atree_Node)
   with Pre  => Node_In_Atree (Tree, Node),
        Post => not Bounded_Stack.Is_Empty (S) and
                Top_In_Atree_Node (S, Tree) = Node
   is
   begin
      pragma Assume (Bounded_Stack.Count (S) < Bounded_Stack.Stack_Count'Last,
                     "A Stack_Size of 32 allows the traversal of " &
                     "balanced tree with more distinct nodes " &
                     "than can be handled by the gnat front-end");
      Bounded_Stack.Push (S, Node);
      pragma Assume (Top_In_Atree_Node (S, Tree) = Node,
                     "The Node pushed on the stack is the top of the stack");
      pragma Warnings (Off, "unused variable ""Tree""",
                      Reason => "Tree is only used in proof context");
   end Push_In_Atree_Node;
   pragma Warnings (On, "unused variable ""Tree""");
   pragma Inline (Push_In_Atree_Node);

   --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the Tree.
   procedure Pop_In_Atree_Node (S : in out Bounded_Stack.Stack;
                                Tree : A_Tree;
                                Node : out Atree_Node)
   with Pre  => not Bounded_Stack.Is_Empty (S),
        Post => Node_In_Atree (Tree, Node) and
                Bounded_Stack.Count (S) = Bounded_Stack.Count (S'Old) - 1
   is
   begin
      Bounded_Stack.Pop (S, Node);
      pragma Assume (Node_In_Atree (Tree, Node),
                     "The exclusive use of Push_In_Tree ensures all " &
                     "pushed nodes are In_Tree, so, " &
                     "all nodes popped by Pop_In_Tree_Node will also be.");

      pragma Warnings (Off, "unused variable ""Tree""",
                      Reason => "Tree is only used in proof context");
   end Pop_In_Atree_Node;
   pragma Warnings (On, "unused variable ""Tree""");
   pragma Inline (Pop_In_Atree_Node);

   --  Local subprograms
   procedure New_Node (Key      : Key_Type;
                       The_Node : out Atree_Node)
   is
   begin
      Tree_Abs.Add_Node (The_Node, Key);
   end New_Node;

   function Get_Child (Is_Right : Boolean;
                       Node     : Atree_Node;
                       Tree     : A_Tree)
                       return Atree_Node
   with Pre  => Node_In_Atree (Tree, Node),
        Post => (if Get_Child'Result /= Empty_Node then
                    Node_In_Atree (Tree, Get_Child'Result))
   is
      Result : Atree_Node;
   begin
      if Is_Right then
         Result := Tree_Abs.Right (Node);
      else
         Result := Tree_Abs.Left (Node);
      end if;
      return Result;
   end Get_Child;
   pragma Inline (Get_Child);

   procedure Set_Branch (Is_Right   : Boolean;
                         Node       : in out Atree_Node;
                         Set_Node   : Atree_Node;
                         Tree       : in out A_Tree)
   with Pre  => Node_In_Atree (Tree, Node) and
                Node_In_Atree (Tree, Set_Node)
   is
   begin
      if Is_Right then
         Tree_Abs.Set_Right
           (N      => Node,
            Branch => Set_Node);
      else
         Tree_Abs.Set_Left
           (N      => Node,
            Branch => Set_Node);
      end if;
      Tree.Toggle := not Tree.Toggle;
   end Set_Branch;
   pragma Inline (Set_Branch);

   procedure Find (Tree      : A_Tree;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Visited    : out Bounded_Stack.Stack)
   with Pre  => Populated (Tree),
     Post => (if Found then
                (Tree_Abs.Key (Top_In_Atree_Node (Visited, Tree)) = Key))
   --  Found is true ony if a node with the given Key is present.
   --  If Found, the top of the Visited stack is the Atree_Node
   --  in the tree which contains the Key.
   is

      Current_Node  : Atree_Node;

      Current_Key   : Key_Type;

      --  A Child of the current node.
      Child         : Atree_Node;
      --  Direction: Left = False, Right = True
      Is_Right      : Boolean;

   begin
      Child := Empty_Node;
      --  Clear the visited stack - the Tree is being searced from its root.
      Bounded_Stack.Clear (Visited);

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
         Push_In_Atree_Node (Visited, Tree, Current_Node);

         Current_Key := Tree_Abs.Key (Current_Node);
         Found := Current_Key = Key;
         pragma Assert (if Found then Tree_Abs.In_Tree (Current_Node));
         if not Found then
            --  Take the right branch if the Key value is greater
            --  than the Current_Node Key, otherwise take the left branch.
            Is_Right := Tree_Abs.Key (Current_Node) < Key;
            Child := Get_Child (Is_Right, Current_Node, Tree);
         end if;

         pragma Loop_Invariant
           (not Bounded_Stack.Is_Empty (Visited) and
                (if Found then
                      Node_In_Atree (Tree, Current_Node) and
                      Tree_Abs.Key (Top_In_Atree_Node (Visited, Tree)) = Key));

         exit when Found or else not Tree_Abs.In_Tree (Child);

         --  Traverse the tree: the Current_Node is set to one of its
         --  children.
         Current_Node := Child;
      end loop;
      pragma Assert (if Found then Node_In_Atree (Tree, Current_Node));
      --  The Tree.Visited stack will not be empty.
      --  if Found is True, the Tree contains a node with the
      --  matching Key.  The Tree_Node on the top of the Visted
      --  is the node with the given Key.
      --  If Found is False, the tree does not contain a node
      --  with a matching Key. The Tree_Node at the top of
      --  Visited stack will contain the Tree_Node which will be the
      --  Parent of a Tree_Node of a node containing
      --  the Key if it were to be added into the Tree.
   end Find;

   procedure Skew (Sub_Root   : in out Atree_Node;
                   Tree       : in out A_Tree)
   with Pre => Node_In_Atree (Tree, Sub_Root),
        Post => Node_In_Atree (Tree, Sub_Root)
   is
      T_In : constant A_Tree := Tree
      with Ghost;
      Left_Child : Atree_Node;
   begin
      Left_Child := Tree_Abs.Left (Sub_Root);
      --  No action is performed if the levels of the sub-root and left nodes
      --  are not equal.
      if Node_In_Atree (Tree, Left_Child) and then
        Tree_Abs.Level (Left_Child) = Tree_Abs.Level (Sub_Root)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Tree_Abs.Set_Left
           (N      => Sub_Root,
            Branch => Tree_Abs.Right (Left_Child));
         Tree_Abs.Set_Right
           (N      => Left_Child,
            Branch => Sub_Root);
         --  The root now becomes the left child.
         Sub_Root := Left_Child;
      end if;
      Tree.Toggle := not Tree.Toggle;
   end Skew;

   procedure Split (Sub_Root : in out Atree_Node;
                    Tree : in out A_Tree)
   with Pre  => Node_In_Atree (Tree, Sub_Root),
        Post => Node_In_Atree (Tree, Sub_Root)
   is
      T_In : constant A_Tree := Tree
      with Ghost;
      Right_Child       : Atree_Node;
      Right_Right_Child : Atree_Node;
   begin
      Right_Child  := Tree_Abs.Right (Sub_Root);
      if Node_In_ATree (Tree, Right_Child) then
         Right_Right_Child := Tree_Abs.Right (Right_Child);
      else
         Right_Right_Child := Empty_Node;
      end if;

      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Node_In_Atree (Tree, Right_Child) and then
         Node_In_Atree (Tree, Right_Right_Child) and then
        Tree_Abs.Level (Right_Right_Child) = Tree_Abs.Level (Sub_Root)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the ruight child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.
         Tree_Abs.Set_Right
           (N      => Sub_Root,
            Branch => Tree_Abs.Left (Right_Child));
         Tree_Abs.Set_Left
           (N      => Right_Child,
            Branch => Sub_Root);
         --  The root now becomes the right child.
         Sub_Root := Right_Child;
         pragma Assume (Tree_Abs.Level (Sub_Root) < Natural'Last,
                        "The Level cannot exceed the number of nodes");
         --  Increment the level of the new root.
         Tree_Abs.Set_Level (Sub_Root,
                             Tree_Abs.Level (Sub_Root) + 1);
      end if;
      Tree.Toggle := not Tree.Toggle;
   end Split;

   procedure Rebalance (Sub_Root : in out Atree_Node;
                        Tree     : in out A_Tree;
                        Visited  : in out Bounded_Stack.Stack)
   with Pre  => Node_In_Atree (Tree, Sub_Root),
     Post => Node_In_Atree (Tree, Sub_Root)
   is
      Current_Node : Atree_Node;
      Top_Node     : Atree_Node;
      --  The parent of the Current_Node
      Parent       : Atree_Node;
      Is_Right     : Boolean;
      Stack_Count  : Natural;
   begin
      --  The following two initalizing statements avoid
      --  flow errors using SPARK 2005 Examiner.
      Is_Right := False;
      Parent := Empty_Node;

      Current_Node := Sub_Root;
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Tree.Visited stack.
      Stack_Count := Bounded_Stack.Count (Visited);
      for Stack_Top in reverse Natural range 1 .. Stack_Count
      loop
         pragma Loop_Invariant (Node_In_Atree (Tree, Current_Node) and
                                not Bounded_Stack.Is_Empty (Visited) and
                                  Stack_Top = Bounded_Stack.Count (Visited));
         --  Make the Current_Node equal to the Tree_Node at the top of
         --  the stack.
         Pop_In_Atree_Node (Visited, Tree, Top_Node);
         Current_Node := Top_Node;
         pragma Assert (Node_In_Atree (Tree, Current_Node));

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Current_Node
            --  As the Current_Node has a parent, determine
            --  whether the Current_Node is a left or right child of
            --  its parent.
            Parent := Top_In_Atree_Node (Visited,  Tree);
            --  This boolean expression determines which branch of
            --  the parent has the the Current_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Node.
            Is_Right :=
              Tree_Abs.Right (Parent) = Top_Node;
         end if;

         --  Perform the Anderson Tree Skew and Split operations on
         --  the Current_Node.  The Current_Node
         --  may be changed by Skew and Split.
         Skew (Current_Node, Tree);
         Split (Current_Node, Tree);
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
               Tree       => Tree);
         end if;
         --  SPARK 2014 does not treat Tree_Type as completely private.
         --  It knows it is a record type with a component of an access type.
         --  It therefore assumes that it may be changed indirectly.
         --  Skew, Split and Set_Branch all maintain Persistence but SPARK 2014
         --  does not assume this. Should a private type be really private?
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      Sub_Root := Current_Node;
   end Rebalance;

   procedure Trace_To_Left_Leaf (E    : in out Enumerator;
                                 Tree : A_Tree)
   with Pre => not Bounded_Stack.Is_Empty (E.Visited)
   is
      Current_Node : Atree_Node;
   begin
      Current_Node :=
        Tree_Abs.Left (Top_In_Atree_Node (E.Visited, Tree));
      while Node_In_Atree (Tree, Current_Node) loop
            Push_In_Atree_Node (E.Visited, Tree, Current_Node);
            Current_Node := Tree_Abs.Left (Current_Node);
      end loop;
   end Trace_To_Left_Leaf;

   procedure Init_Enumerator (Tree      : A_Tree;
                              Enum       : out Enumerator)
   with Pre => Tree_Abs.In_Tree (Tree.Root)
   is
   begin
      Enum.ATree := Tree;
      Bounded_Stack.New_Stack (Enum.Visited);
      Push_In_Atree_Node (Enum.Visited, Tree, Tree.Root);
      Trace_To_Left_Leaf (Enum, Tree);
   end Init_Enumerator;

  --------------
   -- New_Tree --
   --------------

   procedure New_A_Tree (Tree : out A_Tree)
   is
   begin
      Tree.Root      := Empty_Node;
      Tree.Count     := 0;
      Tree.Toggle    := False;
   end New_A_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree      : in out A_Tree;
      Key       : Key_Type;
      Inserted  : out Boolean)
   is
      Visited        : Bounded_Stack.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
      Insert_Node    : Atree_Node;
      Current_Node   : Atree_Node;
       --  A Child of the Current Node.
      Child          : Atree_Node;
      Subroot        : Atree_Node;
   begin
      if not Populated (Tree) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         --  Set_Count (ATree, 1);
         Tree.Count := 1;
         Tree_Abs.Add_Node
           (N       => Insert_Node,
            The_Key => Key);
         --  The new node is the root of the new tree
         Tree.Root := Insert_Node;
      else
         --  Make sure that the tree does not already include the key.
         Find (Tree, Key, Key_Found, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
          else
            Inserted := True;
            Tree.Count := Tree.Count + 1;
            Current_Node := Top_In_Atree_Node (Visited, Tree);
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Tree_Abs.Key (Current_Node) < Key;

            --  Add a new child node to extend the tree
            Tree_Abs.Add_Node
              (N       => Child,
               The_Key => Key);
            Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        Set_Node   => Child,
                        Tree       => Tree);
            Subroot := Tree.Root;
            Rebalance (Subroot, Tree, Visited);
            Tree.Root := Subroot;
         end if;
      end if;
   end Insert;

   -----------------------
   -- Insert_With_Value --
   -----------------------

   procedure Insert_With_Value
     (Tree          : in out A_Tree;
      Key           : Key_Type;
      Insert_Value  : Value_Type;
      Inserted      : out Boolean;
      Value_At_Node : out Value_Type)
   is
      Visited        : Bounded_Stack.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
      Insert_Node    : Atree_Node;
      Current_Node   : Atree_Node;
       --  A Child of the Current Node.
      Child          : Atree_Node;
      Subroot        : Atree_Node;
   begin
      if not Populated (Tree) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         Tree.Count := 1;
         Tree_Abs.Add_Node
           (N       => Insert_Node,
            The_Key => Key);
         Tree_Abs.Set_Value (Insert_Node, Insert_Value);
         --  The new node is the root of the new tree
         Tree.Root := Insert_Node;
         Value_At_Node := Insert_Value;
      else
         --  Make sure that the tree does not already include the key.
         Find (Tree, Key, Key_Found, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
            --  The node with the key is on the top of the visited stack.
            --  Get its value.
            Value_At_Node :=
              Tree_Abs.Value (Top_In_Atree_Node (Visited, Tree));
         else
            Inserted := True;
            Tree.Count := Tree.Count + 1;
            Current_Node := Top_In_Atree_Node (Visited, Tree);
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Tree_Abs.Key (Current_Node) < Key;

            --  Add a new child node to extend the tree
            Tree_Abs.Add_Node
              (N       => Child,
               The_Key => Key);
            Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        Set_Node   => Child,
                        Tree       => Tree);
            Value_At_Node := Insert_Value;
            Subroot := Tree.Root;
            Rebalance (Subroot, Tree, Visited);
            Tree.Root := Subroot;
         end if;
      end if;
   end Insert_With_Value;

   -------------------------------
   -- Last_Underlying_Tree_Node --
   -------------------------------

   function Last_Underlying_Tree_Node (Dummy : Atree_Node) return Atree_Node is
      (Tree_Abs.Last_Node_In_Tree (Dummy));

   -------------------------------------
   -- Clear_Underlying_Tree_From_Node --
   -------------------------------------

   procedure Clear_Underlying_Tree_From_Node (Node : in out Atree_Node)
   is
   begin
      Tree_Abs.Clear_Tree_Below_Node (Node);
   end Clear_Underlying_Tree_From_Node;


   procedure Next_Node (E : in out Enumerator; Node : out Atree_Node) is
      Right_Child : Atree_Node;
   begin
      if not Bounded_Stack.Is_Empty (E.Visited) then
         Pop_In_Atree_Node (E.Visited, E.ATree, Node);
         Right_Child := Tree_Abs.Right (Node);
         if  Node_In_Atree (E.ATree, Right_Child) then
            Push_In_Atree_Node (E.Visited, E.ATree, Right_Child);
            Trace_To_Left_Leaf (E, E.ATree);
         end if;
      else
         Node := Empty_Node;
      end if;
   end Next_Node;
   pragma Inline (Next_Node);

   --------------
   -- Next_Key --
   --------------

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type) is
      Node : Atree_Node;
   begin
      Next_Node (E, Node);
      if Node /= Empty_Node then
         Key := Tree_Abs.Key (Node);
      else
         Key := Null_Key;
      end if;
   end Next_Key;
   pragma Inline (Next_Key);

   ------------------------
   -- Next_Key_And_Value --
   ------------------------

   procedure Next_Key_And_Value (E        : in out Enumerator;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type)
   is
      Node : Atree_Node;
   begin
      Next_Node (E, Node);
      if Node /= Empty_Node then
         Key := Tree_Abs.Key (Node);
         Its_Value := Tree_Abs.Value (Node);
      else
         Key := Null_Key;
         Its_Value := Null_Value;
      end if;
   end Next_Key_And_Value;
   pragma Inline (Next_Key_And_Value);

   ----------------
   -- Equal_Keys --
   ----------------

  function Equal_Keys (ATree_1, ATree_2 : A_Tree) return Boolean
   is
      Enum_1       : Enumerator;
      Enum_2       : Enumerator;
      Key_1        : Key_Type;
      Key_2        : Key_Type;
      Equal        : Boolean;
   begin
      Equal := ATree_1.Count = ATree_2.Count;
      if Equal then
         Init_Enumerator (ATree_1, Enum_1);
         Init_Enumerator (ATree_2, Enum_2);
         loop
            Next_Key (Enum_1, Key_1);
            Next_Key (Enum_2, Key_2);
            Equal := Key_1 = Key_2;
            exit when not Equal or else Key_1 = Null_Key;
         end loop;
      end if;
      return Equal;
   end Equal_Keys;

   ---------------------------
   -- Equal_Keys_And_Values --
   ---------------------------

  function Equal_Keys_And_Values (ATree_1, ATree_2 : A_Tree) return Boolean
   is
      Enum_1       : Enumerator;
      Enum_2       : Enumerator;
      Key_1        : Key_Type;
      Key_2        : Key_Type;
      Value_1      : Value_Type;
      Value_2      : Value_Type;
      Equal        : Boolean;
   begin
      Equal := ATree_1.Count = ATree_2.Count;
      if Equal then
         Init_Enumerator (ATree_1, Enum_1);
         Init_Enumerator (ATree_2, Enum_2);
         loop
            Next_Key_And_Value (Enum_1, Key_1, Value_1);
            Next_Key_And_Value (Enum_2, Key_2, Value_2);
            Equal := Key_1 = Key_2 and then Value_1 = Value_2;
            exit when not Equal or else Key_1 = Null_Key;
         end loop;
      end if;
      return Equal;
   end Equal_Keys_And_Values;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean
   is
      Visited : Bounded_Stack.Stack;
      Found   : Boolean;
   begin
      Find (ATree, Key, Found, Visited);
      return Found;
   end Is_Present;

   ------------
   -- Value --
   ------------

   function Value (ATree : A_Tree; Key : Key_Type) return Value_Type
   is
      Visited : Bounded_Stack.Stack;
      Found   : Boolean;
      Result  : Value_Type;
   begin
      Find (ATree, Key, Found, Visited);
      if Found then
         Result := Tree_Abs.Value (Top_In_Atree_Node (Visited, ATree));
      else
         Result := Null_Value;
      end if;
      return Result;
   end Value;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth (ATree : A_Tree) return Natural
   is
      Result : Natural;
   begin
      if Empty_Tree (ATree) then
         Result := 0;
      else
         Result := Tree_Abs.Level (ATree.Root);
      end if;
      return Result;
   end Tree_Depth;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator (ATree : A_Tree) return Enumerator
   is
      Result : Enumerator;
   begin
      Result.ATree := ATree;
      Bounded_Stack.New_Stack (Result.Visited);
      Push_In_Atree_Node (Result.Visited, ATree, ATree.Root);
      Trace_To_Left_Leaf (Result, ATree);
      return Result;
   end New_Enumerator;

end Atrees;
