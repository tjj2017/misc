with SPARK_2014.Multi_Atree.Tree_Abs;
package body SPARK_2014.Multi_Atree with
   Refined_State => (Status => Refined_Status)
is
   Refined_Status : Pack_Status := Free;

   --  Basic Predicates

   ------------------
   -- Is_Building  --
   ------------------

   function Is_Building return Boolean is (Refined_Status = Constructing) with
     Refined_Global => Refined_Status;

   ----------------
   --  Building  --
   ----------------

   function Building (ATree : A_Tree) return Boolean is
     (ATree.State = Constructing);

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree (ATree : A_Tree) return Boolean is
      (ATree.Root = Empty_Node);

   function Populated (ATree : A_Tree) return Boolean is
      (Tree_Abs.In_Tree (ATree.Root) and
        ATree.Count > 0);

    function In_A_Tree (N : Tree_Node; Tree : A_Tree) return Boolean is
     (N > Empty_Node and Tree.Root > Empty_Node and
        N < Tree.Root + Tree_Node (Tree.Count));

   --  Proof helper subprograms

   --  The contents of the tree are the same but the tree structure may change.
   --  The root of the tree may be reassigned.
   function Retains (AT_Pre, AT_Post : A_Tree) return Boolean is
     (Count (AT_Post) >= Count (AT_Pre) and
          (for all N in Tree_Node => (if In_A_Tree (N, AT_Pre) then
                                          In_A_Tree (N, AT_Post))))
       with Ghost;

   --  The Tree may have a different structure but contains the same nodes.
   function Maintains (AT_Pre, AT_Post : A_Tree) return Boolean is
     (Retains (AT_Pre, AT_Post) and Retains (AT_Post, AT_Pre))
      with Ghost;

   --  The Tree is retained, in addition the root of the tree is unchanged.
   function Persists (AT_Pre, AT_Post : A_Tree) return Boolean is
     (Retains (AT_Pre, AT_Post) and AT_Pre.Root = AT_Post.Root)
   with Ghost;

   ---------------------------------------------------------------------------
  --  Local functions and procedures

    --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the A_Tree.
   function Top_In_Tree_Node (S    : Bounded_Stacks.Stack;
                              Tree : A_Tree) return Tree_Node with
     Pre  => Building (Tree) and not Bounded_Stacks.Is_Empty (S),
     Post => In_A_Tree (Top_In_Tree_Node'Result, Tree)
   is
      Result : Tree_Node;
   begin
      Result := Bounded_Stacks.Top (S);
      pragma Assume (In_A_Tree (Result, Tree),
                     "The exclusive use of Push_In_Tree ensures all " &
                     "pushed nodes are In_Tree, so, " &
                     "all nodes popped by Pop_In_Tree_Node will also be.");

      return Result;
   end Top_In_Tree_Node;
   pragma Inline (Top_In_Tree_Node);

   --  Ensures that each Node pushed on the stack is in the Tree.
   --  With a stack size of 32 a balanced tree would be enormous, it would
   --  have more nodes than the number actually available.
   procedure Push_In_Tree_Node (S    : in out Bounded_Stacks.Stack;
                                Tree : A_Tree;
                                Node : Tree_Node)
   with Pre  => Building (Tree) and In_A_Tree (Node, Tree),
        Post => not Bounded_Stacks.Is_Empty (S) and
                Top_In_Tree_Node (S, Tree) = Node
   is
   begin
      pragma Assume (Bounded_Stacks.Count (S) < Bounded_Stacks.Stack_Count'Last,
                     "A Stack_Size of 32 allows the traversal of " &
                     "balanced tree with more distinct nodes " &
                     "than can be handled by the gnat front-end");
      Bounded_Stacks.Push (S, Node);
      pragma Assume (Top_In_Tree_Node (S, Tree) = Node,
                     "The Node pushed on the stack is the top of the stack");
   end Push_In_Tree_Node;
   pragma Inline (Push_In_Tree_Node);

   --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the Tree.
   procedure Pop_In_Tree_Node (S    : in out Bounded_Stacks.Stack;
                               Tree : A_Tree;
                               Node : out Tree_Node)
   with Pre  => Building (Tree) and not Bounded_Stacks.Is_Empty (S),
        Post => In_A_Tree (Node, Tree) and
                Bounded_Stacks.Count (S) = Bounded_Stacks.Count (S'Old) - 1
   is
   begin
      Bounded_Stacks.Pop (S, Node);
      pragma Assume (In_A_Tree (Node, Tree),
                     "The exclusive use of Push_In_Tree ensures all " &
                     "pushed nodes are In_Tree, so, " &
                     "all nodes popped by Pop_In_Tree_Node will also be.");
   end Pop_In_Tree_Node;
   pragma Inline (Pop_In_Tree_Node);

   procedure New_Node (Key      : Key_Type;
                       Tree     : in out A_Tree;
                       The_Node : out Tree_Node) with
     Pre  => Building (Tree),
     Post => In_A_Tree (The_Node, Tree)
   is
      Dummy_Tree_Parameter : Tree_Node := Tree.Root;
   begin
      Tree_Abs.Add_Node (Dummy_Tree_Parameter, The_Node, Key);
      pragma Assume (In_A_Tree (The_Node, Tree),
                     "The exclusive use of Push_In_Tree ensures all " &
                     "pushed nodes are In_Tree, so, " &
                     "all nodes popped by Pop_In_Tree_Node will also be.");
   end New_Node;

   function Get_Child (Is_Right : Boolean;
                       Node     : Tree_Node;
                       Tree     : A_Tree)
                       return Tree_Node
   with Pre  => Building (Tree) and In_A_Tree (Node, Tree),
        Post => (if Get_Child'Result /= Empty_Node then
                    In_A_Tree (Get_Child'Result, Tree))
   is
      Result : Tree_Node;
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
                         Node       : Tree_Node;
                         Set_Node   : Tree_Node;
                         Tree       : in out A_Tree) with
     Pre  => Building (Tree) and
             In_A_Tree (Node, Tree) and In_A_Tree (Set_Node, Tree),
     Post => Maintains (Tree'Old, Tree)
   is
      Node_To_Set : Tree_Node := Node;
   begin
      if Is_Right then
         Tree_Abs.Set_Right
           (N      => Node_To_Set,
            Branch => Set_Node);
      else
         Tree_Abs.Set_Left
           (N      => Node_To_Set,
            Branch => Set_Node);
      end if;
   end Set_Branch;
   pragma Inline (Set_Branch);

   procedure Find (ATree      : A_Tree;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Visited    : out Bounded_Stacks.Stack) with
     Pre  => Populated (ATree),
     Post => not Bounded_Stacks.Is_Empty (Visited) and
             (if Found then
                ((Tree_Abs.Key (Top_In_Tree_Node (Visited, ATree)) = Key) and
                       Tree_Abs.Key_Is_Present (Key, ATree.Root)))
     --  The Tree is not empty (Populated) so the given Key may be present.
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
      Child := Empty_Node;
      --  Clear the visited stack - the Tree is being searced from its root.
      Bounded_Stacks.Clear (Visited);

      --  If the Tree.Root is not present, the Tree is empty
      --  and the given Key will not be found.
      --  There is nothing more to be done.

      --  The Current_Node is initially set to the root of the tree.
      Current_Node := ATree.Root;
      --  Search the binary tree to find a matching Key or, if it is
      --  not found, locate an appropriate leaf to place the Key.
      --  If Found the node with the matching Key is on the top of the
      --  Tree.Visited stack.
      --  If not Found the top of the Visited stack contains the leaf node
      --  appropriate for the insertion of the key into one of its child
      --  branches.
      loop
         --  A record of nodes visited is held in the Visited stack.
         Push_In_Tree_Node (Visited, ATree, Current_Node);

         Current_Key := Tree_Abs.Key (Current_Node);
         Found := Current_Key = Key;
         pragma Assert (if Found then In_A_Tree (Current_Node, ATree));
         if not Found then
            --  Take the right branch if the Key value is greater
            --  than the Current_Node Key, otherwise take the left branch.
            Is_Right := Tree_Abs.Key (Current_Node) < Key;
            Child := Get_Child (Is_Right, Current_Node, ATree);
         end if;

         pragma Loop_Invariant
           (not Bounded_Stacks.Is_Empty (Visited) and
                (if Found then
                      In_A_Tree (Current_Node, ATree) and
                       Tree_Abs.Key (Top_In_Tree_Node (Visited, ATree)) = Key
                 and Tree_Abs.Key_Is_Present (Key, ATree.Root)));

         exit when Found or else not Tree_Abs.In_Tree (Child);

         --  Traverse the tree: the Current_Node is set to one of its
         --  children.
         Current_Node := Child;
         --# assert not (Found or (not Trees.In_Tree (ATree.Container, Current_Node)));
      end loop;
      pragma Assert (if Found then In_A_Tree (Current_Node, ATree));
      --# check Found -> (Trees.Key (ATree.Container, Current_Node) = Key);
      --  The Tree.Visited stack will not be empty.
      --  if Found is True, the Tree_Store contains an Actual_Node with the
      --  matching Key.  The Tree_Node on the top of the Tree.Visted
      --  stack references this Actual_Node.
      --  If Found is False, the Tree_Store does not contain an Actual_Node
      --  with a matching Key. The Tree_Node at the top of
      --  Tree.Visited stack will contain the Tree_Node which will be the
      --  Parent of a Tree_Node referencing an Actual_Node containing
      --  the Key if it were to be added into the Tree.
   end Find;

   procedure Skew (Sub_Tree_Root : in out Tree_Node;
                   Tree          : in out A_Tree) with
     Pre  => Building (Tree) and In_A_Tree (Sub_Tree_Root, Tree),
     Post => Building (Tree) and Maintains (Tree'Old, Tree)
   is
      T_In : constant A_Tree := Tree
      with Ghost;
      Left_Child : Tree_Node;
   begin
      Left_Child := Tree_Abs.Left (Sub_Tree_Root);
      --  No action is performed if the levels of the root and left nodes
      --  are not equal.
      if In_A_Tree (Left_Child, Tree) and then
        Tree_Abs.Level (Left_Child) = Tree_Abs.Level (Sub_Tree_Root)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Tree_Abs.Set_Left
           (N      => Sub_Tree_Root,
            Branch => Tree_Abs.Right (Left_Child));
         Tree_Abs.Set_Right
           (N      => Left_Child,
            Branch => Sub_Tree_Root);
         Sub_Tree_Root := Left_Child;
      end if;
      pragma Assert (
   end Skew;

   procedure Split (Root : in out Tree_Node;
                    Tree : in out Trees.Tree_Type)
   with Pre  => Trees.In_Tree (Tree, Root),
        Post => Trees.Persists (Tree'Old, Tree) and Trees.In_Tree (Tree, Root)
   is
      T_In : constant Trees.Tree_Type := Tree
      with Ghost;
      Right_Child       : Tree_Node;
      Right_Right_Child : Tree_Node;
   begin
      Right_Child  := Trees.Right (Tree, Root);
      if Trees.In_Tree (Tree, Right_Child) then
         Right_Right_Child := Trees.Right (Tree, Right_Child);
      else
         Right_Right_Child := Trees.Empty_Node;
      end if;

      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Trees.In_Tree (Tree, Right_Child) and then
        Trees.In_Tree (Tree, Right_Right_Child) and then
        Trees.Level (Tree, Right_Right_Child) = Trees.Level (Tree, Root)
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
           (T      => Tree,
            N      => Root,
            Branch => Trees.Left (Tree, Right_Child));
         pragma Assert (Trees.Persists (T_In, Tree));
         Trees.Set_Left
           (T      => Tree,
            N      => Right_Child,
            Branch => Root);
         --  The root now becomes the right child.
         Root := Right_Child;
         pragma Assume (Trees.Level (Tree, Root) < Natural'Last,
                        "The Level cannot exceed the number of nodes");
         --  Increment the level of the new root.
         Trees.Set_Level (Tree,
                          Root,
                          Trees.Level (Tree, Root) + 1);
         pragma Assume (Trees.Persists (T_In, Tree),
                       "Tree is Persistant across Right, Left & Level ops.");
     end if;
   end Split;

   procedure Rebalance (Root    : in out Tree_Node;
                        Tree    : in out Trees.Tree_Type;
                        Visited : in out Bounded_Stacks.Stack)
   with Pre  => Trees.In_Tree (Tree, Root),
        Post => Trees.Persists (Tree'Old, Tree) and Trees.In_Tree (Tree, Root)
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

      Current_Node := Root;
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Tree.Visited stack.
      Stack_Count := Bounded_Stacks.Count (Visited);
      for Stack_Top in reverse Natural range 1 .. Stack_Count
      loop
         pragma Loop_Invariant (Trees.In_Tree (Tree, Current_Node) and
                                not Bounded_Stacks.Is_Empty (Visited) and
                                  Stack_Top = Bounded_Stacks.Count (Visited) and
                                  Trees.Persists (Tree'Loop_Entry, Tree));
         --# assert Stack_Top > 0 and Stack_Top <= Stack_Count and
         --#        Stack_Top = Bounded_Stacks.Count (Visited) and
         --#        not Bounded_Stacks.Is_Empty (Visited) and
         --#        Persists (ATree~, ATree);
         --  Make the Current_Node equal to the Tree_Node at the top of
         --  the stack.
         Pop_In_Tree_Node (Visited, Tree, Top_Node);
         Current_Node := Top_Node;
         pragma Assert (Trees.In_Tree (Tree, Current_Node));

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Current_Node
            --  As the Current_Node has a parent, determine
            --  whether the Current_Node is a left or right child of
            --  its parent.
            Parent := Top_In_Tree_Node (Visited,  Tree);
            --  This boolean expression determines which branch of
            --  the parent has the the Current_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Node.
            Is_Right :=
              Trees.Right (Tree, Parent) = Top_Node;
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
         pragma Assume
           (Trees.Persists (Tree'Loop_Entry, Tree),
            "Split, Skew, Rebalance and Set_Branch retain Persistence");
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      Root := Current_Node;
      --# check Retains (ATree~, ATree);
   end Rebalance;

   procedure Trace_To_Left_Leaf (E    : in out Enumerator;
                                 Tree : Trees.Tree_Type)
   with Pre => not Bounded_Stacks.Is_Empty (E.Visited)
   is
      Current_Node : Tree_Node;
   begin
      Current_Node :=
        Trees.Left (Tree, Top_In_Tree_Node (E.Visited, Tree));
      while Trees.In_Tree (Tree, Current_Node) loop
            Push_In_Tree_Node (E.Visited, Tree, Current_Node);
            Current_Node := Trees.Left (Tree, Current_Node);
      end loop;
   end Trace_To_Left_Leaf;

   procedure Init_Enumerator (ATree      : A_Tree;
                              Enum       : out Enumerator)
   with Pre => Trees.In_Tree (ATree.Container, ATree.Root)
   is
   begin
      Enum.ATree := ATree;
      Bounded_Stacks.New_Stack (Enum.Visited);
      Push_In_Tree_Node (Enum.Visited, ATree.Container, ATree.Root);
      Trace_To_Left_Leaf (Enum, ATree.Container);
   end Init_Enumerator;

  --------------
   -- New_Tree --
   --------------

   procedure New_A_Tree (ATree : out A_Tree; Tree_Container : Trees.Tree_Type)
   is
   begin
      ATree.Container := Tree_Container;
      ATree.Root      := Trees.Empty_Node;
      ATree.Count     := 0;
   end New_A_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (ATree      : in out A_Tree;
      Key        : Key_Type;
      Inserted   : out Boolean)
     with Refined_Post => Trees.Key_Is_Present (ATree.Container, Key) and
                Populated (ATree) and
                (if not Populated (ATree'Old) then
                   Count (ATree) = 1
                     elsif Inserted then
                       Count (ATree) = Count (ATree'Old) + 1
                 else
                    Count (ATree) = Count (ATree'Old))
   is
      Tree_Container : Trees.Tree_Type ;
      Visited        : Bounded_Stacks.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
      Insert_Node    : Trees.Tree_Node;
      Current_Node   : Trees.Tree_Node;
       --  A Child of the Current Node.
      Child          : Trees.Tree_Node;
   begin
      --# accept W, 444, "There cannot be Natural'Last tree nodes",
      --#                "Definition of Populated and Count";
      --# assume ATree.Count < Natural'Last;
      --# assume (not Populated (ATree)) -> ATree.Count = 0;
      --# assume ATree.Count = Count (ATree);
      --# end accept;
      if not Populated (ATree) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         --  Set_Count (ATree, 1);
         ATree.Count := 1;
--         --# check Count (ATree) = 1;
         Trees.Add_Node
           (T       => ATree.Container,
            N       => Insert_Node,
            The_Key => Key);
         --  The new node is the root of the new tree
         ATree.Root := Insert_Node;
      else
         --  Make sure that the tree does not already include the key.
         Find (ATree, Key, Key_Found, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
          else
            Inserted := True;
            --# check Trees.in_Tree (ATree.Container, ATree.Root);
            --  Inc_Count (ATree);
            ATree.Count := ATree.Count + 1;
            Tree_Container := ATree.Container;
            Current_Node := Top_In_Tree_Node (Visited, Tree_Container);
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Trees.Key (Tree_Container, Current_Node) < Key;

            --  Add a new child node to extend the tree
            Trees.Add_Node
              (T       => Tree_Container,
               N       => Child,
               The_Key => Key);
            Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        Set_Node   => Child,
                        Tree       => Tree_Container);
            --# check Populated (ATree);
            -- Now rebalance the tree
            --# accept F, 10, Visited, "Visited must be an in out paramter ",
            --#                        "as it is updated by Rebalance_Tree",
            --#                        " but its final value is unrequired";
            pragma Warnings (Off, """Visited""",
                      Reason => "Visited must be an in out paramter " &
                                 "as it is updated by Rebalance_Tree" &
                                 " but its final value is unrequired");
            Rebalance (ATree.Root, Tree_Container, Visited);
            --  SPARK 2014 does not treat Tree_Type as completely private.
            --  It knows it is a record type with a component of an access type.
            --  It therefore assumes that it may be changed indirectly.
            --  Set_Branch and Reblance maintain Persistence and therefore
            --  the precence of a key but SPARK 2014 does not assume this.
            --  Should a private type be really private?
            pragma Assume (Trees.Persists (ATree.Container, Tree_Container),
                          "Set_Branch and Rebalance maintain Persistence");
            pragma Assume (Trees.Key_Is_Present (Tree_Container, Key),
                           "Set_Branch and Rebalance maintain keys");
            ATree.Container := Tree_Container;

            pragma Warnings (On, """Visited""");
         end if;
      end if;
      --# accept W, 444, "The Key is in the Tree",
      --#                "Definition of Count";
      --# assume Is_Present (ATree, Key);
      --# assume Count (ATree) = ATree.Count;
      --# end accept;
   end Insert;

   -----------------------
   -- Insert_With_Value --
   -----------------------

   procedure Insert_With_Value
     (ATree         : in out A_Tree;
      Key           : Key_Type;
      Value         : Value_Type;
      Inserted      : out Boolean;
      Value_At_Node : out Value_Type)
     with Refined_Post => Trees.Key_Is_Present (ATree.Container, Key) and
                Populated (ATree) and
                (if not Populated (ATree'Old) then
                   Count (ATree) = 1
                     elsif Inserted then
                       Count (ATree) = Count (ATree'Old) + 1
                 else
                    Count (ATree) = Count (ATree'Old))
   is
      Tree_Container : Trees.Tree_Type ;
      Visited        : Bounded_Stacks.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
      Insert_Node    : Trees.Tree_Node;
      Current_Node   : Trees.Tree_Node;
       --  A Child of the Current Node.
      Child          : Trees.Tree_Node;
   begin
      --# accept W, 444, "There cannot be Natural'Last tree nodes",
      --#                "Definition of Populated and Count";
      --# assume ATree.Count < Natural'Last;
      --# assume (not Populated (ATree)) -> ATree.Count = 0;
      --# assume ATree.Count = Count (ATree);
      --# end accept;
      if not Populated (ATree) then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         ATree.Count := 1;
         Trees.Add_Node
           (T       => ATree.Container,
            N       => Insert_Node,
            The_Key => Key);
         Value_At_Node := Value;
         Trees.Set_Value (ATree.Container, Insert_Node, Value_At_Node);
         --  The new node is the root of the new tree
         ATree.Root := Insert_Node;
      else
         --  Make sure that the tree does not already include the key.
         Find (ATree, Key, Key_Found, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
            --  The node with the key is on the top of the visited stack.
            --  Get its value.
            Value_At_Node :=
              Trees.Value (ATree.Container,
                           Top_In_Tree_Node (Visited, ATree.Container));
         else
            Inserted := True;
            ATree.Count := ATree.Count + 1;
            Tree_Container := ATree.Container;
            Current_Node := Top_In_Tree_Node (Visited, Tree_Container);
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Trees.Key (Tree_Container, Current_Node) < Key;

            --  Add a new child node to extend the tree
            Trees.Add_Node
              (T       => Tree_Container,
               N       => Child,
               The_Key => Key);
            Set_Branch (Is_Right   => Is_Right,
                        Node       => Current_Node,
                        Set_Node   => Child,
                        Tree       => Tree_Container);
            Value_At_Node := Value;
            Trees.Set_Value (Tree_Container, Child, Value_At_Node);
            --# check Trees.in_Tree (ATree.Container, ATree.Root);
            --# check Populated (ATree);
            -- Now rebalance the tree
            --# accept F, 10, Visited, "Visited must be an in out paramter ",
            --#                        "as it is updated by Rebalance_Tree",
            --#                        " but its final value is unrequired";
            pragma Warnings (Off, """Visited""",
                      Reason => "Visited must be an in out paramter " &
                                 "as it is updated by Rebalance_Tree" &
                                 " but its final value is unrequired");
            Rebalance (ATree.Root, Tree_Container, Visited);
               --  SPARK 2014 does not treat Tree_Type as completely private.
            --  It knows it is a record type with a component of an access type.
            --  It therefore assumes that it may be changed indirectly.
            --  Set_Branch and Reblance maintain Persistence and therefore
            --  the precence of a key but SPARK 2014 does not assume this.
            --  Should a private type be really private?
            pragma Assume (Trees.Persists (ATree.Container, Tree_Container),
                          "Set_Branch and Rebalance maintain Persistence");
            ATree.Container := Tree_Container;

            pragma Warnings (On, """Visited""");
         end if;
      end if;
      pragma Assume (Trees.Key_Is_Present (ATree.Container, Key),
                     "Set_Branch and Rebalance maintain keys");
      --# accept W, 444, "The Key is in the Tree",
      --#                "Definition of Count";
      --# assume Is_Present (ATree, Key);
      --# assume Count (ATree) = ATree.Count;
      --# end accept;
   end Insert_With_Value;

   ------------------
   -- Clear_A_Tree --
   ------------------

   procedure Clear_A_Tree (ATree       : in out A_Tree)
   is
   begin
      Trees.Clear (ATree.Container, ATree.Root);
      --  Tree := Null_A_Tree;
      --  SPARK Examiner 2005 Simplifier does not infer that
      --  Tree := Null_A_Tree implies Tree.Root = Trees.Empty_Node;
      ATree.Root := Trees.Empty_Node;
      ATree.Count := 0;
   end Clear_A_Tree;

   ---------------
   -- Next_Node --
   ---------------

   procedure Next_Node (E    : in out Enumerator; Node : out Tree_Node) is
      Right_Child : Trees.Tree_Node;
      Tree_Container : Trees.Tree_Type;
   begin
      if not Bounded_Stacks.Is_Empty (E.Visited) then
         Tree_Container := E.ATree.Container;
         Pop_In_Tree_Node (E.Visited, Tree_Container, Node);
         Right_Child := Trees.Right (Tree_Container, Node);
         if  Trees.In_Tree (Tree_Container, Right_Child) then
            Push_In_Tree_Node (E.Visited, Tree_Container, Right_Child);
            Trace_To_Left_Leaf (E, Tree_Container);
            E.ATree.Container := Tree_Container;
         end if;
      else
         Node := Trees.Empty_Node;
      end if;
   end Next_Node;
   pragma Inline (Next_Node);

    -------------
   -- Is_Equal --
   --------------

  function Is_Equal (ATree_1, ATree_2 : A_Tree) return Boolean
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
      Equal := ATree_1.Count = ATree_2.Count;
      if Equal then
         Init_Enumerator (ATree_1, Enum_1);
         Init_Enumerator (ATree_2, Enum_2);
         loop
            Next_Node (Enum_1, Current_1);
            Next_Node (Enum_2, Current_2);
            Present_1 := Trees.In_Tree (ATree_1.Container, Current_1);
            Present_2 := Trees.In_Tree (ATree_2.Container, Current_2);
            Both_Present := Present_1 and Present_2;
            if Both_Present then
               Equal := Trees.Key (ATree_1.Container, Current_1) =
                 Trees.Key (ATree_2.Container, Current_2);
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
     (ATree : A_Tree; Key : Key_Type) return Boolean
     with Refined_Post=>
       (if Is_Present'Result then
          Trees.Key_Is_Present (ATree.Container, Key))
   is
      Visited : Bounded_Stacks.Stack;
      Found   : Boolean;
   begin
      --# accept F, 10, Visited, "Visited stack is used by Find" &
      --#        F, 33, Visited, "Final value of Visited is not required";
      pragma Warnings (Off, """Visited""",
                      Reason => "Visited must be an in out paramter " &
                                 "as it is updated by Find" &
                                 " but its final value is unrequired");
      Find (ATree, Key, Found, Visited);
      pragma Warnings (On, """Visited""");
      pragma Assert (if Found then Trees.Key_Is_Present (ATree.Container, Key));
      return Found;
   end Is_Present;

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
         Result := Trees.Level (ATree.Container, ATree.Root);
      end if;
      return Result;
   end Tree_Depth;

   -----------
   -- Count --
   -----------

   function Count (ATree : A_Tree) return Natural is (ATree.Count);

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator (ATree : A_Tree) return Enumerator
   is
      Result : Enumerator;
   begin
      Result.ATree := ATree;
      Bounded_Stacks.New_Stack (Result.Visited);
      Push_In_Tree_Node (Result.Visited, ATree.Container, ATree.Root);
      Trace_To_Left_Leaf (Result, ATree.Container);
      return Result;
   end New_Enumerator;

end SPARK_2014.Multi_Atree;
