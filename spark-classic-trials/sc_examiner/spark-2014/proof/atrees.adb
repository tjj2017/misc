package body Atrees with
SPARK_Mode
is
   type Direction is (Left, Right);

   Null_Index : constant Node_Index := Tree.Null_Index;

   procedure Init_Host_Tree (Host : out Host_Tree) with
     Refined_Post => Tree.Empty_Tree (Tree.Tree (Host))
   is
   begin
      Tree.Init (Tree.Tree (Host));
   end Init_Host_Tree;

   --  A proof function only use to show logical association between an
   --  A_Tree object and a Host_Tree_Object.
   --  The body is hidden from SPARK so that the proof does not assume
   --  that In_Host is always True.
   function In_Host (Atree : A_Tree; Host : Host_Tree) return Boolean is
     (not Tree.Empty_Tree (Tree.Tree (Host)) and then
      Tree.In_Tree (Tree.Tree (Host), Atree.Root));


   --  A proof function only use to show logical association between an
   --  An Enumerator an A_Tree objec and its Host_Tree_Object.
   --  The body is hidden from SPARK so that the proof does not assume
   --  that In_Host is always True.
   function Enumerator_Of_Tree (E : Enumerator;
                                A : A_Tree;
                                T : Host_Tree)
                                return Boolean with
     SPARK_Mode => Off
   is
   begin
      return (True);
   end Enumerator_Of_Tree;

   -----------
   -- Count --
   -----------

   function Count (Atree : A_Tree) return Node_Count is (Atree.Count);


   function Populated (Atree : A_Tree; Host : Host_Tree) return Boolean is
     (not Tree.Empty_Tree (Tree.Tree (Host)) and then In_Host (Atree, Host)
          and then
        (Count (Atree) > 0 and Atree.Root /= Null_Index));

   --  A logical function to state that a Node_Index references a node
   --  within the given A_Tree.
   function In_Atree (Atree : A_Tree; Host : Host_Tree; Node : Node_Index)
                       return Boolean is
     (Tree.In_Tree (Tree.Tree (Host), Node) and Count (Atree) > 0) with
   Ghost;

   function Key_Index_Of_Node (Atree : A_Tree;
                               Host  : Host_Tree;
                               Index : Node_Index) return Key_Index with
     Pre  => In_Atree (Atree, Host, Index) and then Count (Atree) > 0,
     Post => (for some I in Key_Index range 1 .. Key_Index (Count (Atree)) =>
                Indexed_Key (Atree, Host, I) =
                  Tree.Key (Tree.Tree (Host), Index) and then
                Key_Index_Of_Node'Result = I)
   is
      E : Enumerator := New_Enumerator (Atree, Host);
   begin

      for I in Key_Index range 1 .. Key_Index (Count (Atree)) loop



   --  Proof helper subprograms

   --  Pushing exclusively using Push_In_Tree_Node ensures that
   --  every Node on the stack is in the Tree.
   function Top_In_Atree_Index (S     : Stack.Stack;
                                Atree : A_Tree;
                                Host  : Host_Tree) return Node_Index with
     Pre  => not Stack.Is_Empty (S),
     Post => In_Atree (Atree, Host, Top_In_Atree_Index'Result),
     Inline
   is
      Result : Node_Index;
   begin
      Result := Stack.Top (S);
      pragma Assume (In_Atree (Atree, Host, Result),
                     "The exclusive use of Push_In_Atree_Index ensures all " &
                     "pushed node indices are In_Atreeree, so, " &
                      "all node indices at the top of the stack will be " &
                       "In_Atree.");

      return Result;
--        pragma Warnings (Off, "unused variable ""Tree""",
--                        Reason => "Tree is only used in proof context");
   end Top_In_Atree_Index;
--     pragma Warnings (On, "unused variable ""Tree""");

   --  Ensures that each Node pushed on the stack is in the A_Tree.
   --  It is assumed the Stack_Size Parameter of this generic package is
   --  large enough to traverse a balanced tree.
   --  A Stack_Size of 32 should be sufficient to traverse a tree containing
   --  2**32-1 nodes.
   procedure Push_In_Atree_Index (S     : in out Stack.Stack;
                                  Atree : A_Tree;
                                  Host  : Host_Tree;
                                  Index : Node_Index) with
     Pre  => In_Atree (Atree, Host, Index),
     Post => not Stack.Is_Empty (S) and
             Top_In_Atree_Index (S, Atree, Host) = Index,
     Inline
   is
   begin
      pragma Assume (Stack.Count (S) < Stack.Stack_Count'Last,
                     "A Stack_Size of 32 allows the traversal of " &
                     "balanced tree with more distinct nodes " &
                     "than can be handled by the gnat front-end");
      Stack.Push (S, Index);
      pragma Assume (Top_In_Atree_Index (S, Atree, Host) = Index,
                     "The Node_Index pushed on the stack is the " &
                       "top of the stack");
--        pragma Warnings (Off, "unused variable ""Tree""",
--                        Reason => "Tree is only used in proof context");
   end Push_In_Atree_Index;

   --  Pushing exclusively using Push_In_Tree_Index ensures that
   --  every Node_Index on the stack is in the A_Tree.
   procedure Pop_In_Atree_Index (S     : in out Stack.Stack;
                                 Atree : A_Tree;
                                 Host  : Host_Tree;
                                 Index : out Node_Index) with
     Pre  => not Stack.Is_Empty (S),
     Post => In_Atree (Atree, Host, Index) and
             Stack.Count (S) = Stack.Count (S'Old) - 1,
     Inline
   is
   begin
      Stack.Pop (S, Index);
      pragma Assume (In_Atree (Atree, Host, Index),
                     "The exclusive use of Push_In_ATree_Index ensures " &
                     "all pushed node indices are In_ATree, so, " &
                     "all nodes popped by Pop_In_ATree_Index will also be.");
--
--        pragma Warnings (Off, "unused variable ""Tree""",
--                        Reason => "Tree is only used in proof context");
   end Pop_In_Atree_Index;
--     pragma Warnings (On, "unused variable ""Tree""");
--     pragma Inline (Pop_In_Atree_Node);

   --  Local subprograms

   function Get_Child (Is_Right : Boolean;
                       Atree    : A_Tree;
                       Host     : Host_Tree;
                       Index    : Node_Index)
                       return Node_Index with
     Pre => In_Atree (Atree, Host, Index),
     Inline
   is
      Result : Node_Index;
   begin
      if Is_Right then
         Result := Tree.Right (Tree.Tree (Host), Index);
      else
         Result := Tree.Left (Tree.Tree (Host), Index);
      end if;
      return Result;
   end Get_Child;

   procedure Set_Branch (Is_Right   : Boolean;
                         Atree      : A_Tree;
                         Host       : in out Host_Tree;
                         Index      : Valid_Node_Index;
                         Set_Index  : Valid_Node_Index) with
     Pre  => In_Atree (Atree, Host, Index),
     Post => In_Atree (Atree, Host, Index) and In_Atree (Atree, Host, Index),
     Inline
   is
   begin
      if Is_Right then
         Tree.Set_Right
           (T      => Tree.Tree (Host),
            I      => Index,
            Branch => Set_Index);
      else
         Tree.Set_Left
           (T      => Tree.Tree (Host),
            I      => Index,
            Branch => Set_Index);
      end if;
   end Set_Branch;

   procedure Add_Atree_Index (Atree       : in out A_Tree;
                              Host        : in out Host_Tree;
                              Key         : Key_Type;
                              Added_Index : out Node_Index) with
     Pre  => In_Host (Atree, Host),
     Post => In_Atree (Atree, Host, Added_Index) and
             Tree.Key (Tree.Tree (Host), Added_Index) = Key and
             Count (Atree) = Count (Atree'Old) + 1,
     Inline
   is
   begin
      Tree.Add_Node (Tree.Tree (Host), Added_Index, Key);
      Atree.Count := Atree.Count + 1;
   end Add_Atree_Index;

   --  If Found, the top of the Visited stack is the Node_Index
   --  of the node in the tree which contains the Key.
   --  If Found is False, the tree does not contain a node
   --  with a matching Key and The Node_Index at the top of the Visited stack
   --  will contain the Node_Index of the Parent of a of the node containing
   --  the Key if it were to be added into the Tree.
   procedure Find (Atree      : A_Tree;
                   Host       : Host_Tree;
                   Root_Index : Node_Index;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Visited    : out Stack.Stack) with
     Pre  => (not Tree.Empty_Tree (Tree.Tree (Host)) and
              Root_Index /= Null_Index) and then
             In_ATree (Atree, Host, Root_Index),
     Post => not Stack.Is_Empty (Visited) and then
             (if Found then
                Tree.Key (Tree.Tree (Host),
                          Top_In_Atree_Index(Visited, Atree, Host)) = Key and
                  (for some N in Node_Index =>
                       Tree.Key (Tree.Tree (Host), N) =
                     Key and N = Top_In_Atree_Index (Visited, Atree, Host)))
   is

      Current_Index : Node_Index;

      Current_Key   : Key_Type;

      --  A Child of the current node.
      Child         : Node_Index;
      --  Direction: Left = False, Right = True
      Is_Right      : Boolean;

   begin
      --  Clear the visited stack - the Tree is being searced from its root.
      Stack.Clear (Visited);

      --  The Current_Node_Index is initially the root index of the Atree.
      Current_Index := Root_Index;
      --  Search the binary tree to find a matching Key or, if it is
      --  not found, locate an appropriate leaf to place the Key.
      --  If Found the node with the matching Key is on the top of the
      --  Tree.Visited stack.
      --  If not Found the top of the Visited stack contains the leaf node
      --  appropriate for the insertion of the key into one of its child
      --  branches.

      loop
         --  Avoid a conditional flow error.
         Child := Null_Index;
         --  A record of nodes visited is held in the Visited stack.
         Push_In_Atree_Index (Visited, Atree, Host, Current_Index);

         -- Loop_Invariant
         --# assert Bounded_Stacks.Not_Empty (Visited) and
         --# (Found -> (Basic_Tree.Key (Host,
         --#              Bounded_Stacks.Top (Visited)) = Key));

         Current_Key := Tree.Key (Tree.Tree (Host), Current_Index);
         Found := Current_Key = Key;
         if not Found then
            --  Take the right branch if the Key value is greater
            --  than the Current_Node Key, otherwise take the left branch.
            Is_Right := Key > Current_Key;
            Child := Get_Child (Is_Right, Atree, Host, Current_Index);
         end if;

         exit when Found or else Child = Null_Index;

         --  Traverse the tree: the Current_Index is set to the Node_Index
         --  of one of its children.
         Current_Index := Child;
      end loop;
   end Find;

   --  The Skew operation on an Andersson tree after inserting a node.
   procedure Skew (Atree          : A_Tree;
                   Host           : in out Host_Tree;
                   Sub_Root_Index : in out Node_Index) with
     Pre  => In_Atree (Atree, Host, Sub_Root_Index),
     Post => In_Atree (Atree, Host, Sub_Root_Index)
   is
      Left_Child : Node_Index;
   begin
      Left_Child := Tree.Left (Tree.Tree (Host), Sub_Root_Index);
      --  No action is performed if the levels of the sub-root and left nodes
      --  are not equal.
      if Tree.Level (Tree.Tree (Host), Left_Child) =
        Tree.Level (Tree.Tree (Host), Sub_Root_Index)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Andersson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root_index.
         Tree.Set_Left
           (T      => Tree.Tree (Host),
            I      => Left_Child,
            Branch => Tree.Right (Tree.Tree (Host), Left_Child));
         --  The root_index now becomes the left child.
         Sub_Root_Index := Left_Child;
      end if;
   end Skew;

   --  The Split operation on an Andersson tree after inserting a node.
   procedure Split (Atree          : A_Tree;
                    Host           : in out Host_Tree;
                    Sub_Root_Index : in out Node_Index) with
     Pre  => In_Atree (Atree, Host, Sub_Root_Index),
     Post => In_Atree (Atree, Host, Sub_Root_Index)
   is
      Right_Child       : Node_Index;
      Right_Right_Child : Node_Index;
   begin
      Right_Child  := Tree.Right (Tree.Tree (Host), Sub_Root_Index);
      if Right_Child /= Null_Index then
         Right_Right_Child := Tree.Right (Tree.Tree (Host), Right_Child);
      else
         Right_Right_Child := Null_Index;
      end if;

      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Right_Child /= Null_Index and then Right_Right_Child /= Null_Index
         and then
          Tree.Level (Tree.Tree (Host), Right_Right_Child) =
            Tree.Level (Tree.Tree (Host), Sub_Root_Index)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the right child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.
         Tree.Set_Right
           (T      => Tree.Tree (Host),
            I      => Sub_Root_Index,
            Branch => Tree.Left (Tree.Tree (Host), Right_Child));
         Tree.Set_Left
           (T      => Tree.Tree (Host),
            I      => Right_Child,
            Branch => Sub_Root_Index);
         --  The root now becomes the right child.
         Sub_Root_Index := Right_Child;
         --  Increment the level of the new root.
         Tree.Set_Level (Tree.Tree (Host),
                         Sub_Root_Index,
                         Tree.Level (Tree.Tree (Host), Sub_Root_Index) + 1);
      end if;
   end Split;

   --  Rebalance Andersson tree after am insertion.
   procedure Rebalance (Atree          : A_Tree;
                        Host           : in out Host_Tree;
                        Sub_Root_Index : in out Node_Index;
                        Visited        : in out Stack.Stack)
   is
      Current_Index : Node_Index;
      Top_Index     : Node_Index;
      --  The Node_Index of the parent of the node referenced by Current_Index
      Parent       : Node_Index;
      Is_Right     : Boolean;
      Stack_Count  : Natural;
   begin
      --  The following two initalizing statements avoid
      --  flow errors using SPARK 2005 Examiner.
      Is_Right := False;
      Parent := Null_Index;

      Current_Index := Sub_Root_Index;
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Visited stack.
      Stack_Count := Stack.Count (Visited);
      for Stack_Top in reverse Natural range 1 .. Stack_Count
      loop
         --# assert (Bounded_Stacks.Not_Empty (Visited) and
         --#            Stack_Top = Bounded_Stacks.Count (Visited));
         --  Make the Current_Index equal to the Node_Index at the top of
         --  the stack.
         Pop_In_Atree_Index (Visited, Atree, Host, Top_Index);
         Current_Index := Top_Index;

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the Node_Index of the parent of the node
            --  referenced by the Current_Index, the current node.
            --  As the current node has a parent, determine
            --  whether the current node is a left or right child of
            --  its parent.
            Parent := Top_In_Atree_Index (Visited, Atree, Host);
            --  This boolean expression determines which branch of
            --  the parent has the the Current_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Node.
            Is_Right := Tree.Right (Tree.Tree (Host), Parent) = Top_Index;
         end if;

         --  Perform the Andersosn Tree Skew and Split operations on
         --  the Current_Node.  The Current_Node
         --  may be changed by Skew and Split.
         Skew (Atree, Host, Current_Index);
         Split (Atree, Host, Current_Index);
         --  Update the parent node to point to its new child.
         if Current_Index /= Top_Index and then Stack_Top > 1 then
            --  The value of the Current_Index
            --  may have changed and the stack has the
            --  parent of the Current_Index at the top of the
            --  visited stack.
            --  The branch that has the Current_Index as its child
            --  has to be patched up to contain the new value of
            --  Current_Node as its child.
            Set_Branch
              (Is_Right  => Is_Right,
               Atree     => Atree,
               Host      => Host,
               Index     => Parent,
               Set_Index => Current_Index);
         end if;
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      Sub_Root_Index := Current_Index;
   end Rebalance;

   --  Trace the Atree to locate its lowest value Key which is its leftmost
   --  node.
   procedure Trace_To_Left_Leaf (Atree : A_Tree;
                                 Host  : Host_Tree;
                                 E     : in out Enumerator) with
     Pre => not Stack.Is_Empty (E.Visited)
   is
      Current_Index : Node_Index;
   begin
      Current_Index := Tree.Left (Tree.Tree (Host),
                                  Top_In_Atree_Index (E.Visited, Atree, Host));
      while Current_Index /= Null_Index loop
            Push_In_Atree_Index (E.Visited, Atree, Host, Current_Index);
            Current_Index := Tree.Left (Tree.Tree (Host), Current_Index);
      end loop;
   end Trace_To_Left_Leaf;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator (Atree : A_Tree; Host : Host_Tree)
                            return Enumerator
   is
      Result : Enumerator;
   begin
      Result.Node_Issue := 0;
      Stack.New_Stack (Result.Visited);
      Push_In_Atree_Index (Result.Visited, Atree, Host, Atree.Root);
      Trace_To_Left_Leaf (Atree, Host, Result);
      if not Stack.Is_Empty (Result.Visited) then
         Result.Node_Issue := 1;
      end if;
      pragma Assume (Enumerator_Of_Tree (Result, Atree, Host),
                     "The New_Enumerator is associated with the given " &
                    "A_Tree and Host_Tree.");
      return Result;
   end New_Enumerator;

  -----------------
   -- New_A_Tree --
   ----------------

   procedure New_A_Tree (Atree : out A_Tree; Host : in out Host_Tree) with
     Refined_Post => not Tree.Empty_Tree (Tree.Tree (Host)) and
                     In_Host (Atree, Host)
   is
      New_Index : Node_Index;
   begin
      Tree.Add_Node
        (T         => Tree.Tree (Host),
         New_Index => New_Index,
         The_Key   => Null_Key);
      Atree := A_Tree'
        (Count => 0,
         Base  => New_Index,
         Root  => Null_Index);
   end New_A_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert (Atree     : in out A_Tree;
                     Host      : in out Host_Tree;
                     Key       : Key_Type;
                     Inserted  : out Boolean)
   is
      Visited        : Stack.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
      Insert_Index   : Node_Index;
       --  A Child of the currently Indexed node.
      Child          : Node_Index;
      Subroot_Index  : Node_Index;
   begin
      if Count (Atree) = 0 then
         --  First node of Atree - Enter a new node with level 1 into the store
         Inserted := True;
         Atree.Count := 1;
         --  An empty node has already been placed in the host tree by
         --  New_A_Tree.  Its index is in Atree.Base.
         --  Update the node with the given key and make it the root.
         Atree.Root := Atree.Base;
         Tree.Set_Key (Tree.Tree (Host), Atree.Root, Key);
         Tree.Set_Level (Tree.Tree (Host), Atree.Root, 1);
      else
         --  Make sure that the tree does not already include the key.
         Find (Atree      => Atree,
               Host       => Host,
               Root_Index => Atree.Root,
               Key        => Key,
               Found      => Key_Found,
               Visited    => Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
          else
            Inserted := True;
            Insert_Index := Top_In_Atree_Index (Visited, Atree, Host);
            --  A right branch if the value of Key is greater
            --  to the Top Value, otherwise take the left branch.
            --  There are no duplicate Keys.
            Is_Right := Key > Tree.Key (Tree.Tree (Host), Insert_Index);

            --  Add a new child node to extend the tree
            Add_Atree_Index
              (Atree       => Atree,
               Host        => Host,
               Added_Index => Child,
               Key         => Key);
            Set_Branch
              (Is_Right  => Is_Right,
               Atree     => Atree,
               Host      => Host,
               Index     => Insert_Index,
               Set_Index => Child);
            Subroot_Index := Atree.Root;

            pragma Warnings (Off, """Visited""",
                             Reason =>
                            "Visited stack is not needed after rebalancing");
            Rebalance
              (Atree          => Atree,
               Host           => Host,
               Sub_Root_Index => Subroot_Index,
               Visited        => Visited);
            pragma Warnings (On, """Visited""");

            Atree.Root := Subroot_Index;
         end if;
      end if;
   end Insert;

   -----------------------
   -- Insert_With_Value --
   -----------------------

   procedure Insert_With_Value (Atree         : in out A_Tree;
                                Host          : in out Host_Tree;
                                Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type)
   is
      Visited        : Stack.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
      Insert_Index   : Node_Index;
       --  A Child of the currently Indexed node.
      Child          : Node_Index;
      Subroot_Index  : Node_Index;
   begin
      if Count (Atree) = 0 then
         --  First node of Atree - Enter a new node with level 1 into the store
         Inserted := True;
         Atree.Count := 1;
         --  An empty node has already been placed in the host tree by
         --  New_A_Tree.  Its index is in Atree.Base.
         --  Update the node with the given key and make it the root.
         Atree.Root := Atree.Base;
         Tree.Set_Key (Tree.Tree (Host), Atree.Root, Key);
         Tree.Set_Value (Tree.Tree (Host), Atree.Root, Insert_Value);
         Tree.Set_Level (Tree.Tree (Host), Atree.Root, 1);
         Value_At_Node := Insert_Value;
      else
         --  Make sure that the tree does not already include the key.
         Find (Atree      => Atree,
               Host       => Host,
               Root_Index => Atree.Root,
               Key        => Key,
               Found      => Key_Found,
               Visited    => Visited);
         if Key_Found then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
            --  The index to the node with the key is on the top of the
            --  visited stack. Get its value.
            Value_At_Node :=
              Tree.Value (Tree.Tree (Host),
                          Top_In_Atree_Index (Visited, Atree, Host));
         else
            Inserted := True;
            Insert_Index := Top_In_Atree_Index (Visited, Atree, Host);
            --  A right branch if the value of Key is greater
            --  to the Top Value, otherwise take the left branch.
            --  There are no duplicate Keys.
            Is_Right := Key > Tree.Key (Tree.Tree (Host), Insert_Index);

            --  Add a new child node to extend the tree
            Add_Atree_Index
              (Atree       => Atree,
               Host        => Host,
               Added_Index => Child,
               Key         => Key);
            --  Add the Value to the node.
            Tree.Set_Value
              (T          => Tree.Tree (Host),
               I          => Child,
               Node_Value => Insert_Value);
            --  Make the new Child node a child of the node indexed by
            --  Current_Index.
            Set_Branch
              (Is_Right  => Is_Right,
               Atree     => Atree,
               Host      => Host,
               Index     => Insert_Index,
               Set_Index => Child);

            Value_At_Node := Insert_Value;
            Subroot_Index := Atree.Root;

            pragma Warnings (Off, """Visited""",
                             Reason =>
                            "Visited stack is not needed after rebalancing");
            Rebalance
              (Atree          => Atree,
               Host           => Host,
               Sub_Root_Index => Subroot_Index,
               Visited        => Visited);
            pragma Warnings (On, """Visited""");

            Atree.Root := Subroot_Index;
         end if;
      end if;
   end Insert_With_Value;

   procedure Next_Node_Index (Atree : A_Tree;
                              Host  : Host_Tree;
                              E     : in out Enumerator;
                              Index : out Node_Index) with
     Pre => In_Host (Atree, Host),
     Inline
   is
      Right_Child : Node_Index;
   begin
      if not Stack.Is_Empty (E.Visited) then
         Pop_In_Atree_Index (E.Visited, Atree, Host, Index);
         Right_Child := Tree.Right (Tree.Tree (Host), Index);
         if Right_Child /= Null_Index then
            Push_In_Atree_Index (E.Visited, Atree, Host, Right_Child);
            Trace_To_Left_Leaf (Atree, Host, E);
         end if;
         E.Node_Issue := E.Node_Issue + 1;
      else
         E.Node_Issue := 0;
         Index := Null_Index;
      end if;
   end Next_Node_Index;

   --------------
   -- Next_Key --
   --------------

   procedure Next_Key (E : in out Enumerator;
                       Atree : A_Tree;
                       Host : Host_Tree;
                       Key : out Key_Type)
   is
      Next_Index : Node_Index;
   begin
      if E.Node_Issue > 0 and E.Node_Issue < Atree.Count then
         Next_Node_Index (Atree, Host, E, Next_Index);
         if Next_Index /= Null_Index then
            Key := Tree.Key (Tree.Tree (Host), Next_Index);
         else
            Key := Null_Key;
         end if;
      else
         Key := Null_Key;
      end if;
      pragma Assume (Enumerator_Of_Tree (E, Atree, Host),
                     "Obtaining the next key does not affect " &
                       "the association with Atree and Host.");

   end Next_Key;
   pragma Inline (Next_Key);

   ------------------------
   -- Next_Key_And_Value --
   ------------------------

   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Atree     : A_Tree;
                                 Host      : Host_Tree;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type)
   is
      Next_Index : Node_Index;
   begin
      Next_Node_Index (Atree, Host, E, Next_Index);
      if Next_Index /= Null_Index then
         Key := Tree.Key (Tree.Tree (Host), Next_Index);
         Its_Value := Tree.Value (Tree.Tree (Host), Next_Index);
      else
         Key := Null_Key;
         Its_Value := Null_Value;
      end if;
      --# accept F, 30, Atree, "Atree is used in pre and post condition.";
   end Next_Key_And_Value;
   pragma Inline (Next_Key_And_Value);

   ----------------
   -- Equal_Keys --
   ----------------

   function Equal_Keys (Atree_1, Atree_2 : A_Tree;
                        Host_1, Host_2 : Host_Tree) return Boolean
   is
      Enum_1       : Enumerator;
      Enum_2       : Enumerator;
      Key_1        : Key_Type;
      Key_2        : Key_Type;
      Equal        : Boolean;
   begin
      Equal := Atree_1.Count = Atree_2.Count;
      if Equal then
         Enum_1 := New_Enumerator (Atree_1, Host_1);
         Enum_2 := New_Enumerator (Atree_2, Host_2);
         loop
            Next_Key (E     => Enum_1,
                      Atree => Atree_1,
                      Host  => Host_1,
                      Key   => Key_1);
            Next_Key (E     => Enum_2,
                      Atree => Atree_2,
                      Host  => Host_2,
                      Key   => Key_2);
            Equal := Key_1 = Key_2;
            exit when not Equal or else Key_1 = Null_Key;
         end loop;
      end if;
      return Equal;
   end Equal_Keys;

   ---------------------------
   -- Equal_Keys_And_Values --
   ---------------------------

  function Equal_Keys_And_Values (Atree_1, Atree_2 : A_Tree;
                                   Host_1, Host_2 : Host_Tree) return Boolean
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
         Enum_1 := New_Enumerator (Atree_1, Host_1);
         Enum_2 := New_Enumerator (Atree_2, Host_2);
         loop
            Next_Key_And_Value (E     => Enum_1,
                                Atree => Atree_1,
                                Host  => Host_1,
                                Key   => Key_1,
                                Its_Value => Value_1);
            Next_Key_And_Value (E     => Enum_2,
                                Atree => Atree_2,
                                Host  => Host_2,
                                Key   => Key_2,
                                Its_Value => Value_2);

            Equal := Key_1 = Key_2 and then Value_1 = Value_2;
            exit when not Equal or else Key_1 = Null_Key;
         end loop;
      end if;
      return Equal;
   end Equal_Keys_And_Values;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present (Atree : A_Tree; Host : Host_Tree; Key : Key_Type)
                        return Boolean
   is
      Visited : Stack.Stack;
      Found   : Boolean;
   begin
      pragma Warnings (Off, """Visited""",
                             Reason =>
                         "Visited stack is not needed after finding key");
      Find (Atree, Host, Atree.Root, Key, Found, Visited);
      pragma Warnings (On, """Visited""");

      pragma Assert (if Found then
                        Tree.Key (Tree.Tree (Host),
                       Top_In_Atree_Index (Visited, Atree, Host)) = Key);
      pragma Assume (Found =
                       (for some I in Key_Index range
                          1 .. Key_Index (Count (Atree)) =>
                            Indexed_Key (Atree, Host, I) = Key),
                    "The Key has been found it must have a Key_Index");

      pragma Assert (Found =
                       (for some I in Key_Index range
                          1 .. Key_Index (Count (Atree)) =>
                       Indexed_Key (Atree, Host, I) = Key));

      return Found;
   end Is_Present;

   ------------
   -- Value --
   ------------

   function Value (ATree : A_Tree; Host : Host_Tree; Key : Key_Type)
                   return Value_Type
   is
      Visited : Stack.Stack;
      Found   : Boolean;
      Result  : Value_Type;
   begin
      Find (Atree, Host, Atree.Root, Key, Found, Visited);
      if Found then
         Result := Tree.Value (Tree.Tree (Host),
                               Top_In_Atree_Index (Visited, Atree, Host));
      pragma Assert (for all I in Key_Index range
                       1 .. Key_Index (Count(Atree)) =>
                         Indexed_Key (Atree, Host, I) /= Null_Key);
      pragma Assert (for some I in Key_Index range
                       1 .. Key_Index (Count(Atree)) =>
                         Value_At_Key
                       (Atree => Atree,
                        Host  => Host,
                        Key   => Indexed_Key (Atree, Host, I)) = Result);
      else
         Result := Null_Value;
      end if;
      pragma Assert (Result = Null_Value or else
                     Result = Tree.Value (Tree.Tree (Host),
                       Top_In_Atree_Index (Visited, Atree, Host)));
      return Result;
   end Value;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth (Atree : A_Tree; Host : Host_Tree) return Natural
   is
      Result : Natural;
   begin
      if Atree.Count = 0 then
         Result := 0;
      else
         Result := Tree.Level (Tree.Tree (Host), Atree.Root);
      end if;
      return Result;
   end Tree_Depth;

   function Indexed_Key (Atree : A_Tree; Host : Host_Tree; Index : Key_Index)
                         return Key_Type
   is
      E : Enumerator := New_Enumerator (Atree, Host);
      The_Key : Key_Type := Null_Key;
   begin
      pragma Assert (Count (Atree) > 0 and Index > 0 and
                       Index <= Key_Index (Count (Atree)));
      for I in 1 .. Index loop
         pragma Loop_Invariant (Enumerator_Of_Tree (E, Atree, Host));
         Next_Key (E, Atree, Host, The_Key);
      end loop;
      pragma Assert (Count (Atree) > 0 and Index > 0 and
                       Index <= Key_Index (Count (Atree)));
      return The_Key;
   end Indexed_Key;

   function Current_Indexed_Key (E : Enumerator;
                                 A : A_Tree;
                                 T : Host_Tree) return Key_Index is
      (Key_Index (E.Node_Issue));

   function Value_At_Key (Atree : A_Tree; Host : Host_Tree; Key : Key_type)
                          return Value_Type
   is
      Visited : Stack.Stack;
      Found : Boolean;
   begin
      Find
        (Atree      => Atree,
         Host       => Host,
         Root_Index => Atree.Root,
         Key        => Key,
         Found      => Found,
         Visited    => Visited);
      return (if Found then
                 Tree.Value (Tree.Tree (Host), Stack.Top (Visited))
              else
                 Null_Value);
   end Value_At_Key;


end Atrees;
