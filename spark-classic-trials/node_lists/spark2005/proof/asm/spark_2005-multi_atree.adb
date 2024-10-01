with SPARK_2005.Multi_Atree.Tree_Abstraction,
     SPARK_2005.Multi_Atree.Stack_Ops;
package body SPARK_2005.Multi_Atree
--# own Status is Refined_Status;
is
   Refined_Status : Pack_Status;

   ------------------
   -- A_Tree_Init  --
   ------------------

   procedure A_Tree_Init
   --# global out Refined_Status;
  is
   begin
      Refined_Status := Free;
      Tree_Abstraction.Init;
   end A_Tree_Init;

   --  Basic Predicates

   ------------------
   -- Is_Building  --
   ------------------

   function Is_Building return Boolean
   --# global Refined_Status;
   is
   begin
      return (Refined_Status = Constructing);
   end Is_Building;

   ----------------
   --  Building  --
   ----------------

   function Building (ATree : A_Tree) return Boolean
   --# global Refined_Status;
   is
   begin
      return (Is_Building and ATree.State = Constructing);
   end Building;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree (ATree : A_Tree) return Boolean is
   begin
      return  (ATree.Root = Empty_Node);
   end Empty_Tree;

  function In_A_Tree (N : Tree_Node; Tree : A_Tree) return Boolean
   --# return IAT => IAT -> (N in Valid_Tree_Node and
   --#                          Tree_Abstraction.In_Tree (N));
   is
   begin
      return (Tree_Abstraction.In_Tree (N) and
                Tree_Abstraction.In_Tree (Tree.Root) and
                N >= Tree.Root and (N - Tree.Root) + 1 = Tree_Node (Tree.Count));
   end In_A_Tree;

   function Populated (ATree : A_Tree) return Boolean is
   begin
      return (Tree_Abstraction.In_Tree (ATree.Root) and
                In_A_Tree (ATree.Root, ATree) and
                ATree.Root in Valid_Tree_Node and
                  (ATree.Count /= 0));
   end Populated;

    --# function Node_Key (Node : Tree_Node; ATree : A_Tree) return Key_Type;
   --# return Tree_Abstraction.Key (Node);

   --  The contents of the tree are the same but the tree structure may change.
   --  The root of the tree may be reassigned.
   --# function Retains (AT_Pre, AT_Post : A_Tree) return Boolean;
   --# return (AT_Post.Count >= AT_Pre.Count and
   --#        (for all N in Tree_Node =>
   --#            (In_A_Tree (N, AT_Pre) -> In_A_Tree (N, AT_Post))));

   --  The Trees may have a different structure but contains the same nodes.
   --# function Maintains (AT_1, AT_2 : A_Tree) return Boolean;
   --# return  (Retains (AT_1, AT_2) and Retains (AT_2, AT_1));

   --  The Tree is retained, in addition the root of the tree is unchanged.
   --# function Persists (AT_Pre, AT_Post : A_Tree) return Boolean;
   --# return (Retains (AT_Pre, AT_Post) and AT_Pre.Root = AT_Post.Root);

   ---------------------------------------------------------------------------
  --  Local functions and procedures

    --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the A_Tree.
   function Top_In_A_Tree_Node (S : Stack; Tree : A_Tree)
                                return Valid_Tree_Node
   --# pre  not Stack_Ops.Is_Empty (S);
   --# return Top => In_A_Tree (Top, Tree) and
   --#               Tree_Abstraction.In_Tree (Top);
   is
      Result : Tree_Node;
   begin
      Result := Stack_Ops.Top (S);
      --# check In_A_Tree (Result, Tree);
      pragma Assert (In_A_Tree (Result, Tree),
                     "The exclusive use of Push_In_A_Tree ensures all " &
                     "pushed nodes are In_A_Tree, so, " &
                       "all nodes the stack including the top will also be.");
      --# accept F, 30, Tree, "Only used in proof contexts.";
      --# accept F, 50, Tree, "Tree only used in proof contexts";
      return Result;
   end Top_In_A_Tree_Node;
   pragma Inline (Top_In_A_Tree_Node);

   --  Ensures that each Node pushed on the stack is in the A_Tree.
   --  With a stack size of 32 a balanced tree would be enormous, it would
   --  have more nodes than the number actually available.
   procedure Push_In_A_Tree_Node (S    : in out Stack;
                                Node : Valid_Tree_Node;
                                Tree : A_Tree)
   --# pre In_A_Tree (Node, Tree);
   --# post not Stack_Ops.Is_Empty (S) and
   --#            Top_In_A_Tree_Node (S, Tree) = Node;
   is
   begin
      --  The precondition ensures all nodes pushed on the stack are in the
      --  the A_Tree Tree.
      pragma Assert (Stack_Ops.Count (S) < Multi_Atree.Stack_Count'Last,
                     "A Stack_Size of 32 allows the traversal of " &
                     "balanced tree with more distinct nodes " &
                     "than can be handled by the gnat front-end");
      Stack_Ops.Push (S, Node);
      pragma Assert (Top_In_A_Tree_Node (S, Tree) = Node,
                     "The Node pushed on the stack is the top of the stack");
      --# accept F, 30, Tree, "Only used in proof contexts.";
   end Push_In_A_Tree_Node;
   pragma Inline (Push_In_A_Tree_Node);

   --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the Tree.
   procedure Pop_In_A_Tree_Node (S    : in out Stack;
                                 Node : out Valid_Tree_Node;
                                 Tree  : A_Tree)
   --# pre  not Stack_Ops.Is_Empty (S);
   --# post In_A_Tree (Node, Tree) and
   --#        Stack_Ops.Count (S) = Stack_Ops.Count (S~) - 1;
   is
   begin
      Stack_Ops.Pop (S, Node);
      pragma Assert (In_A_Tree (Node, Tree),
                     "The exclusive use of Push_In_A_Tree ensures all " &
                     "pushed nodes are in the Tree, so, " &
                     "all nodes popped by Pop_In_Tree_Node will also be.");
      --# accept F, 30, Tree, "Only used in proof contexts.";
   end Pop_In_A_Tree_Node;
   pragma Inline (Pop_In_A_Tree_Node);

   function Get_Child (Is_Right : Boolean;
                       Node      : Valid_Tree_Node)
                       return Tree_Node
   --# pre Tree_Abstraction.In_Tree (Node);
   --# return GC => (not Tree_Abstraction.Is_Empty (GC)) ->
   --#               Tree_Abstraction.In_Tree (GC);
   is
      Result : Tree_Node;
   begin
      if Is_Right then
         Result := Tree_Abstraction.Right (Node);
      else
         Result := Tree_Abstraction.Left (Node);
      end if;
      return Result;
   end Get_Child;
   pragma Inline (Get_Child);

   procedure Set_Branch (Is_Right   : Boolean;
                         Node       : in out Valid_Tree_Node;
                         Branch     : Tree_Node)
   --# pre  Tree_Abstraction.In_Tree (Node);
   --# post Tree_Abstraction.In_Tree (Node) and
   --#         Get_Child (Is_Right, Node) = Branch;
   is
   begin
      if Is_Right then
         Tree_Abstraction.Set_Right
           (N      => Node,
            Branch => Branch);
      else
         Tree_Abstraction.Set_Left
           (N      => Node,
            Branch => Branch);
      end if;
      pragma Assert (Get_Child (Is_Right, Node) = Branch,
                     "Whether the left or right child is set, " &
                     "the child is set to Branch");
   end Set_Branch;
   pragma Inline (Set_Branch);

   procedure Find (ATree      : A_Tree;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Visited    : out Stack)
   --# pre  Populated (ATree) and ATree.Root in Valid_Tree_Node;
   --# post not Stack_Ops.Is_Empty (Visited) and
   --#        (Found ->
   --#            Tree_Abstraction.Key (Top_In_A_Tree_Node (Visited, ATree)) = Key);
     --  The Tree is Populated so the given Key may be present.
     --  If found is true, the top of the Tree.Visited stack is the
     --  Tree_Node that references the Actual_Node which contains the Key.
   is
      Current_Target  : Valid_Tree_Node;
      Current_Key     : Key_Type;

      --  A Child of the current node.
      Child           : Tree_Node;
      --  Direction: Left = False, Right = True
      Is_Right        : Boolean;

   begin
      Child := Empty_Node;
      --  Clear the visited stack - the Tree is being searched from its root.
      Stack_Ops.Clear (Visited);

      --  If the Tree.Root is not present, the Tree is empty
      --  and the given Key will not be found.
      --  There is nothing more to be done.

      --  The Current_Node is initially set to the root of the tree.
      Current_Target := ATree.Root;
      --  Search the binary tree to find a matching Key or, if it is
      --  not found, locate an appropriate leaf to place the Key.
      --  If Found the node with the matching Key is on the top of the
      --  Tree.Visited stack.
      --  If not Found the top of the Visited stack contains the leaf node
      --  appropriate for the insertion of the key into one of its child
      --  branches.
      loop
         --  A record of nodes visited is held in the Visited stack.
        Push_In_A_Tree_Node (Visited, Current_Target, ATree);

         Current_Key := Tree_Abstraction.Key (Current_Target);
         Found := Current_Key = Key;
         if not Found then

            --  Take the right branch if the Key value is greater
            --  than the Current_Node Key, otherwise take the left branch.
            Is_Right := Tree_Abstraction.Key (Current_Target) < Key;
            Child := Get_Child (Is_Right, Current_Target);
         end if;

         --  pragma Loop_Invariant
         --    (not Stack_Ops.Is_Empty (Visited) and
         --         (Found ->
         --             (In_A_Tree (Current_Target, ATree) and
         --                Tree_Abstraction.Key
         --            (Top_In_A_Tree_Node (Visited, ATree)) = Key));

         exit when Found or else not In_A_Tree (Child, ATree);

         --  Traverse the tree: the Current_Target is set to one of its
         --  children.
         Current_Target := Child;
      end loop;
      --  The Tree.Visited stack will not be empty.
      --  If Found is True, the ATree contans a node with the
      --  matching Key.  The Tree_Node on the top of the Tree.Visted
      --  stack references this node.
      --  If not Found the top of the Visited stack contains the leaf node
      --  appropriate for the insertion of the key into one of its child
      --  branches.
   end Find;

   procedure Skew (Tree : in out A_Tree)
     --  Tree.Target_Node must be set to the subtree root to be skewed.
   --# global in Refined_Status;
   --# pre  Building (Tree, Refined_Status) and
   --#  In_A_Tree (Tree.Target_Node, Tree) and
   --#              Tree.Target_Node in Valid_Tree_Node;
   --# post Building (Tree, Refined_Status) and Maintains (Tree~, Tree) and
   --#        In_A_Tree (Tree.Target_Node, Tree);
   is
      Left_Child : Tree_Node;
   begin
      Left_Child  := Tree_Abstraction.Left (Tree.Target_Node);
      --  No action is performed if the levels of the root and left nodes
      --  are not equal.
      if In_A_Tree (Left_Child, Tree) and then
        Tree_Abstraction.Level (Left_Child) =
        Tree_Abstraction.Level (Tree.Target_Node)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the Target node left child becomes the right child
         --  of the Target node left node.
         --  The right child of the Target node left child
         --  becomes the current Target, and lastly, the
         --  new Target is set to the original Left_Child.
         Tree_Abstraction.Set_Left
           (N      => Tree.Target_Node,
            Branch => Tree_Abstraction.Right (Left_Child));
         Tree_Abstraction.Set_Right
           (N      => Left_Child,
            Branch => Tree.Target_Node);
         pragma Assert (In_A_Tree (Left_Child, Tree),
                        "Setting a branch does not remove any nodes");
         Tree.Target_Node := Left_Child;
      end if;
      --# accept F, 30, Refined_Status, "Only proof contexts.";
   end Skew;

   procedure Split (Tree : in out A_Tree)
     --  Tree.Target_Node must be set to the subtree root to be split.
     --# global in Refined_Status;
     --# pre  Building (Tree, Refined_Status) and
     --#         In_A_Tree (Tree.Target_Node, Tree) and
     --#      Tree.Target_Node in Valid_Tree_Node;
     --# post Building (Tree, Refined_Status) and Maintains (Tree~, Tree) and
     --#      In_A_Tree (Tree.Target_Node, Tree);
   is
      Right_Child       : Tree_Node;
      Right_Right_Child : Tree_Node;
      Current_Level     : Node_Count;
   begin
      Right_Child := Tree_Abstraction.Right (Tree.Target_Node);
      if In_A_Tree (Right_Child, Tree) then
         Right_Right_Child:= Tree_Abstraction.Right (Right_Child);
      else
         Right_Right_Child := Empty_Node;
      end if;

      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if In_A_Tree (Right_Child, Tree) and then
        In_A_Tree (Right_Right_Child, Tree) and then
        Tree_Abstraction.Level (Right_Right_Child) =
        Tree_Abstraction.Level (Tree.Target_Node)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the right child of the Target becomes the left child
         --  of the right child of the Target. The right child of the
         --  right child of the root becomes the root
         --  the right child of the Target becomes the new Target and its level
         --  is incremented.
         Tree_Abstraction.Set_Right
           (N      => Tree.Target_Node,
            Branch => Tree_Abstraction.Left (Right_Child));
         Tree_Abstraction.Set_Left
           (N      =>  Right_Child,
            Branch => Tree.Target_Node);
         --  The Target now becomes the right child.
         Tree.Target_Node := Right_Child;
         Current_Level := Tree_Abstraction.Level (Tree.Target_Node);
         pragma Assert (Current_Level < Node_Count'Last,
                        "The Level cannot exceed the number of nodes");
         --  Increment the level of the new Target.
         Tree_Abstraction.Set_Level (Tree.Target_Node, Current_Level + 1);
         pragma Assert (In_A_Tree (Tree.Target_Node, Tree),
                       "Setting the level does not remove any nodes");
     end if;
      --# accept F, 30, Refined_Status, "Only used in proof contexts.";
   end Split;

   procedure Rebalance (Tree          : in out A_Tree;
                        Visited       : in out Stack)
   --  Tree.Target_Node must be set to the subtree root to be rebalanced.
   --# global in Refined_Status;
   --# pre  Building (Tree, Refined_Status);
   --# post Building (Tree, Refined_Status) and Maintains (Tree~, Tree);
   is
      Top_Node       : Tree_Node;
      --  The parent and Child of the Target_Node
      Parent         : Tree_Node := Empty_Node;
      Child          : Tree_Node;
      Is_Right       : Boolean := False;
      Stack_Counter  : Stack_Count;
   begin
      --# check Building (Tree, Refined_Status);
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Tree.Visited stack.
      Stack_Counter := Stack_Ops.Count (Visited);
      for Stack_Top in reverse Stack_Count range 1 .. Stack_Counter
      loop
         --  Make the Target_Node equal to the Tree_Node at the top of
         --  the stack.
         pragma Loop_Invariant (not Stack_Ops.Is_Empty (Visited) and
                                  Stack_Top = Stack_Ops.Count (Visited) and
                                  Building (Tree));  --  and
                               --   Maintains (Tree'Loop_Entry, Tree));
         Pop_In_A_Tree_Node (Visited, Top_Node, Tree);
         Tree.Target_Node := Top_Node;
         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Target_Node
            --  As the Target_Node has a parent, determine
            --  whether the Target_Node is a left or right child of
            --  its parent.
            Parent := Top_In_A_Tree_Node (Visited, Tree);
            --  This boolean expression determines which branch of
            --  the parent has the the Target_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Target_Node.
            Is_Right :=
              Tree_Abstraction.Right (Parent) = Top_Node;
         end if;

         --  Perform the Anderson Tree Skew and Split operations on
         --  the from the Target_Node of Tree.  The Target_Node
         --  may be changed by Skew and Split.
         Skew (Tree);
         Split (Tree);
         --  Update the parent node to point to its new child.
         if Tree.Target_Node /= Top_Node and then Stack_Top > 1 then
            --  The value of the Target_Node
            --  may have changed and the stack has the
            --  parent of the Target_Node at the top of the
            --  visited stack.
            --  The branch that had the Target_Node as its child
            --  has to be patched up to contain the new value of
            --  Target_Node as its child.
            --  As the value of the Target_Node may have changed.
            --  The Target_Node becomes the Parent but the current
            --  Target_Node has to be saved, it becomes the Child.
            Child := Tree.Target_Node;
            Tree.Target_Node := Parent;
            Set_Branch
              (Is_Right => Is_Right,
               Node     => Tree.Target_Node,
               Branch   => Child);
         end if;
      end loop;
   end Rebalance;

   procedure Trace_To_Left_Leaf (E    : in out Enumerator)
   --# pre not Stack_Ops.Is_Empty (E.Visited);
   is
      Current_Node : Tree_Node ;
   begin
      Current_Node :=
        Tree_Abstraction.Left (Top_In_A_Tree_Node (E.Visited, E.ATree));
      while In_A_Tree (Current_Node, E.ATree) loop
         Push_In_A_Tree_Node (E.Visited, Current_Node, E.ATree);
         Current_Node := Tree_Abstraction.Left (Current_Node);
      end loop;
   end Trace_To_Left_Leaf;

   procedure Init_Enumerator (ATree      : A_Tree;
                              Enum       : out Enumerator)
   --# pre Populated (ATree) and ATree.Root in Valid_Tree_Node;
   is
   begin
      Enum.ATree := ATree;
      Stack_Ops.New_Stack (Enum.Visited);
      Push_In_A_Tree_Node (Enum.Visited, ATree.Root, ATree);
      Trace_To_Left_Leaf (Enum);
   end Init_Enumerator;

  --------------
   -- New_Tree --
   --------------

   procedure New_A_Tree (ATree : out A_Tree)
   --# global in out Refined_Status;
   is
   begin
      Refined_Status := Constructing;
      ATree := A_Tree'
        (Root        => Empty_Node,
         Target_Node => Empty_Node,
         Count       => 0,
         State       => Constructing);
      --# accept F, 35, Refined_Status, "Only used in precondition.";
   end New_A_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (ATree      : in out A_Tree;
      Key        : Key_Type;
      Inserted   : out Boolean;
      Key_Node   : out Tree_Node)
   --# global in Refined_Status;
   is
      Visited          : Stack;
      Key_Found        : Boolean;
      Is_Right         : Boolean;
   begin
      if not Populated (ATree) then
         pragma Assert (ATree.Count = 0,
                       "Definition of not Populated -> Count = 0");
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         --  Set_Count (ATree, 1);
         ATree.Count := 1;
         Tree_Abstraction.Add_Node
           (N       => Key_Node,
            The_Key => Key);
         --  The new node is the root of the new tree
         ATree.Root := Key_Node;
         ATree.Target_Node := Key_Node;
      else
         --  Make sure that the tree does not already include the key.
         Find (ATree, Key, Key_Found, Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
            Key_Node := Top_In_A_Tree_Node (Visited, ATree);
          else
            Inserted := True;
            ATree.Count := ATree.Count + 1;
            ATree.Target_Node := Top_In_A_Tree_Node (Visited, ATree);
            pragma Assert (Populated (ATree),
                          "Populated is independent of the Target_Node." );
            --  Get the Key of the top node of the stack (the new Target_Node.
            --  A right branch if the value of Key is greater (or equal)
            --  to the Top Value, otherwise take the left branch.
            Is_Right := Tree_Abstraction.Key (ATree.Target_Node) < Key;

            --  Add a new node to extend the tree.
            Tree_Abstraction.Add_Node
              (N       => Key_Node,
               The_Key => Key);

            Set_Branch
              (Is_Right => Is_Right,
               Node     => ATree.Target_Node,
               Branch   => Key_Node);
               --  The Target_Node is set to the newly inserted node.
            pragma Warnings (Off, """Visited""",
                      Reason => "Visited must be an in out paramter " &
                                 "as it is updated by Rebalance_Tree" &
                                 " but its final value is unrequired");
            --# accept F, 10, Visited,
            --# "Must be an out parameter as it is updated but final value ",
            --#  "is unused.";
            Rebalance (ATree, Visited);
            --  Set the Target_Node to the newly inserted node.
            ATree.Target_Node := Key_Node;
            pragma Warnings (On, """Visited""");
         end if;
      end if;
   end Insert;

   ------------------
   -- Clear_A_Tree --
   ------------------

   procedure Clear_A_Tree (ATree : in out A_Tree)
   --# global in out Refined_Status;
   is
   begin
      Tree_Abstraction.Clear_Tree_Below_Node (ATree.Root);
      ATree.Target_Node := Empty_Node;
      ATree.Count := 0;
      ATree.State := Unassigned;
      Refined_Status := Free;
      --# accept F, 35, Refined_Status, "Only used in precondition.";
   end Clear_A_Tree;

   ---------------
   -- Next_Node --
   ---------------

   procedure Next_Node (E    : in out Enumerator; The_Node : out Tree_Node) is
      Right_Child : Tree_Node;
   begin
      if not Stack_Ops.Is_Empty (E.Visited) then
         Pop_In_A_Tree_Node (E.Visited, The_Node, E.ATree);
         Right_Child := Tree_Abstraction.Right (The_Node);
         if  In_A_Tree (Right_Child, E.ATree) then
            Push_In_A_Tree_Node (E.Visited, Right_Child, E.ATree);
            Trace_To_Left_Leaf (E);
         end if;
      else
         The_Node:= Empty_Node;
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
      pragma Assert (ATree_1.Root in Valid_Tree_Node and
                       ATree_2.Root in Valid_Tree_Node,
                     "Both ATree_1 and ATree_2 are Populated.");
      Equal := ATree_1.Count = ATree_2.Count;
      if Equal then
         Init_Enumerator (ATree_1, Enum_1);
         Init_Enumerator (ATree_2, Enum_2);
         loop
            Next_Node (Enum_1, Current_1);
            Next_Node (Enum_2, Current_2);
            --  The_Target_Nodes of Current_1 and Current_2 have the next nodes
            Present_1 := In_A_Tree (Current_1, ATree_1);
            Present_2 := In_A_Tree (Current_2, ATree_2);
            Both_Present := Present_1 and Present_2;
            if Both_Present then
               Equal := Tree_Abstraction.Key (Current_1) =
                 Tree_Abstraction.Key (Current_2);
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
   is
      Visited : Stack;
      Found   : Boolean;
   begin
      pragma Assert (ATree.Root in Valid_Tree_Node,
                     "ATree is Populated");
      pragma Warnings (Off, """Visited""",
                      Reason => "Visited must be an in out paramter " &
                                 "as it is updated by Find" &
                                 " but its final value is unrequired");
      --# accept F, 10, Visited,
      --# "Must be an out parameter as it is updated but final value ",
      --#  "is unused.";
      Find (ATree, Key, Found, Visited);
      pragma Warnings (On, """Visited""");
      --# accept F, 33, Visited,
      --# "Must be an out parameter as it is updated but final value ",
      --#  "is unused.";
      return Found;
   end Is_Present;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth (ATree : A_Tree) return Node_Count
   is
      Result : Node_Count;
   begin
      if Empty_Tree (ATree) then
         Result := 0;
      else
         Result := Tree_Abstraction.Level (ATree.Root);
      end if;
      return Result;
   end Tree_Depth;

   -----------
   -- Count --
   -----------

   function Count (ATree : A_Tree) return Node_Count is
   begin
      return (ATree.Count);
   end Count;

   --------------------
   -- New_Enumerator --
   --------------------

   procedure New_Enumerator (ATree : A_Tree; New_Enum : out Enumerator)
   is
   begin
      pragma Assert (ATree.Root in Valid_Tree_Node,
                     "ATree is Populated");
      New_Enum.ATree := ATree;
      Stack_Ops.New_Stack (New_Enum.Visited);
      Push_In_A_Tree_Node (New_Enum.Visited, ATree.Root, ATree);
      Trace_To_Left_Leaf (New_Enum);
   end New_Enumerator;

end SPARK_2005.Multi_Atree;
