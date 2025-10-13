pragma Ada_2012;
with Tree_With_State;
package body Atrees_With_State is

   --  Local subprograms
   procedure New_Node (Key      : Key_Type;
                       The_Node : out Tree_Node) with
     Global => Tree_With_State.Tree_Store
   is
   begin
      Tree_With_State.Add_Node (The_Node, Key);
   end New_Node;

   function Get_Child (Is_Right : Boolean; Node : Tree_Node) return Tree_Node
   with
     Global => Tree_With_State.Tree_Store,
     Pre    => Node /= Empty_Node
   is
      Result : Tree_Node;
   begin
      if Is_Right then
         Result := Tree_With_State.Right (Node);
      else
         Result := Tree_With_State.Left (Node);
      end if;
      return Result;
   end Get_Child;
   pragma Inline (Get_Child);

   procedure Set_Branch (Is_Right   : Boolean;
                         Node       : Tree_Node;
                         Set_Node   : Tree_Node) with
     Global => (In_Out => Tree_With_State.Tree_Store)
   is
   begin
      if Is_Right then
         Tree_With_State.Set_Right
           (N      => Node,
            Branch => Set_Node);
      else
         Tree_With_State.Set_Left
           (N      => Node,
            Branch => Set_Node);
      end if;
   end Set_Branch;
   pragma Inline (Set_Branch);

   function Get_Key (N : Tree_Node) return Key_Type is
     (Tree_With_State.Key (N));

   ----------
   -- Skew --
   ----------

   procedure Skew (Sub_Root   : in out Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre  => Sub_Root /= Empty_Node
   is
      Left_Child : Tree_Node;
   begin
      Left_Child := Tree_With_State.Left (Sub_Root);
      --  No action is performed if the levels of the sub-root and left nodes
      --  are not equal.
      if Left_Child /= Empty_Node and then
        Tree_With_State.Level (Left_Child) = Tree_With_State.Level (Sub_Root)
      then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Tree_With_State.Set_Left
           (N      => Sub_Root,
            Branch => Tree_With_State.Right (Left_Child));
         Tree_With_State.Set_Right
           (N      => Left_Child,
            Branch => Sub_Root);
         --  The root now becomes the left child.
         Sub_Root := Left_Child;
      end if;
   end Skew;

   -----------
   -- Split --
   -----------

   procedure Split (Sub_Root : in out Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre  => Sub_Root /= Empty_Node
   is
      Right_Child       : Tree_Node;
      Right_Right_Child : Tree_Node;
   begin
      Right_Child  := Tree_With_State.Right (Sub_Root);
      if Right_Child /= Empty_Node then
         Right_Right_Child := Tree_With_State.Right (Right_Child);
      else
         Right_Right_Child := Empty_Node;
      end if;

      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if Right_Child /= Empty_Node and then
         Right_Right_Child /= Empty_Node and then
        Tree_With_State.Level (Right_Right_Child) =
        Tree_With_State.Level (Sub_Root)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the ruight child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.
         Tree_With_State.Set_Right
           (N      => Sub_Root,
            Branch => Tree_With_State.Left (Right_Child));
         Tree_With_State.Set_Left
           (N      => Right_Child,
            Branch => Sub_Root);
         --  The root now becomes the right child.
         Sub_Root := Right_Child;
         pragma Assume (Tree_With_State.Level (Sub_Root) < Natural'Last,
                        "The Level cannot exceed the number of nodes");
         --  Increment the level of the new root.
         Tree_With_State.Set_Level (Sub_Root,
                             Tree_With_State.Level (Sub_Root) + 1);
      end if;
   end Split;

   ---------------
   -- Rebalance --
   ---------------

   procedure Rebalance (Sub_Root : in out Tree_Node;
                        Visited  : in out Bounded_Stack.Stack) with
     Global => (In_Out => Tree_Store),
     Pre  => Sub_Root /= Empty_Node
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
      Parent := Empty_Node;

      Current_Node := Sub_Root;
      --  Rebalance the tree by working back up through the visited
      --  node indices on the Tree.Visited stack.
      Stack_Count := Bounded_Stack.Count (Visited);
      for Stack_Top in reverse Natural range 1 .. Stack_Count
      loop
         pragma Loop_Invariant (Current_Node /= Empty_Node and
                                not Bounded_Stack.Is_Empty (Visited) and
                                  Stack_Top = Bounded_Stack.Count (Visited));
         --  Make the Current_Node equal to the Tree_Node at the top of
         --  the stack.
         Bounded_Stack.Pop (Visited, Top_Node);
         Current_Node := Top_Node;
         pragma Assume (Current_Node /= Empty_Node,
                        "All nodes pushe on the stack are non-empty");

         if Stack_Top > 1 then
            --  There was more than element on the stack - the current
            --  stack top is the parent of the Current_Node
            --  As the Current_Node has a parent, determine
            --  whether the Current_Node is a left or right child of
            --  its parent.
            Parent := Bounded_Stack.Top (Visited);
            pragma Assume (Current_Node /= Empty_Node,
                           "All nodes pushe on the stack are non-empty");

            --  This boolean expression determines which branch of
            --  the parent has the the Current_Node as its child.
            --  False => Left, True => Right.
            --  The value of Is_Right has to be determined before the
            --  call of Skew and Split as these may change the
            --  Current_Node.
            Is_Right :=
              Tree_With_State.Right (Parent) = Top_Node;
         end if;

         --  Perform the Anderson Tree Skew and Split operations on
         --  the Current_Node.  The Current_Node
         --  may be changed by Skew and Split.
         Skew (Current_Node);
         Split (Current_Node);
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
               Set_Node   => Current_Node);
         end if;
      end loop;
      --  The root of the tree after inserting a node and rebalancing.
      Sub_Root := Current_Node;
   end Rebalance;

   procedure Trace_To_Left_Leaf (E : in out Enumerator) with
     Pre => not Bounded_Stack.Is_Empty (E.Visited)
   is
      Current_Node : Valid_Tree_Node;
   begin
      Current_Node :=
        Tree_With_State.Left (Bounded_Stack.Top (E.Visited));
      while Current_Node /= Empty_Node loop
            Bounded_Stack.Push (E.Visited, Current_Node);
            Current_Node := Tree_With_State.Left (Current_Node);
      end loop;
   end Trace_To_Left_Leaf;

   procedure Next_Node (E : in out Enumerator; Node : out Tree_Node) is
      Right_Child : Tree_Node;
   begin
      if not Bounded_Stack.Is_Empty (E.Visited) then
         Bounded_Stack.Pop (E.Visited, Node);
         Right_Child := Tree_With_State.Right (Node);
         if  Right_Child /= Empty_Node then
            Bounded_Stack.Push (E.Visited, Right_Child);
            Trace_To_Left_Leaf (E);
         end if;
      else
         Node := Empty_Node;
      end if;
   end Next_Node;
   pragma Inline (Next_Node);



   --  Subprograms available to child packages.

   procedure Find (Start_Node : Tree_Node;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Visited    : out Bounded_Stack.Stack) with
     Refined_Post => (if Found then
                (Tree_With_State.Key (Bounded_Stack.Top (Visited)) = Key))
   --  Found is true ony if a node with the given Key is present.
   --  If Found, the top of the Visited stack is the Tree_Node
   --  in the tree which contains the Key.
   is

      Current_Node  : Tree_Node;

      Current_Key   : Key_Type;

      --  A Child of the current node.
      Child         : Tree_Node;
      --  Direction: Left = False, Right = True
      Is_Right      : Boolean;

   begin
      Child := Empty_Node;
      --  Clear the visited stack -
      --  the Tree is being searhed .
      Bounded_Stack.Clear (Visited);

      --  If the Start_Node is an Empty_Node, the Tree is empty
      --  and the given Key will not be found.
      --  There is nothing more to be done.

      --  The Current_Node is initially set to the Start_Node.
      Current_Node := Start_Node;
      --  Search the binary tree from Start_Node to find a matching Key or,
      --  if it is not found, locate an appropriate leaf to place the Key.
      --  If Found the node with the matching Key is on the top of the
      --  Tree.Visited stack.
      --  If not Found the top of the Visited stack contains the leaf node
      --  appropriate for the insertion of the key into one of its child
      --  branches.
      loop
         --  A record of nodes visited is held in the Visited stack.
         Bounded_Stack.Push (Visited, Current_Node);

         Current_Key := Tree_With_State.Key (Current_Node);
         Found := Current_Key = Key;
         if not Found then
            --  Take the right branch if the Key value is greater
            --  than the Current_Node Key, otherwise take the left branch.
            Is_Right := Current_Key < Key;
            Child := Get_Child (Is_Right, Current_Node);
         end if;

         pragma Loop_Invariant
           (not Bounded_Stack.Is_Empty (Visited) and
                (if Found then
                      Tree_With_State.Key (Bounded_Stack.Top (Visited)) = Key));

         exit when Found or else Child = Empty_Node;

         --  Traverse the tree: the Current_Node is set to one of its
         --  children.
         Current_Node := Child;
      end loop;
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

   ---------------------
   -- Is_Present_From --
   ---------------------

   function Is_Present_From (Start_Node : Tree_Node; Key: Key_Type)
                             return Boolean
   is
      Visited : Bounded_Stack.Stack;
      Found   : Boolean;
   begin
      Find (Start_Node, Key, Found, Visited);
      return Found;
   end Is_Present_From;

   ----------------
   -- Value_From --
   ----------------

   function Value_From (Start_Node : Tree_Node; Key : Key_Type)
                        return Value_Type
   is
      Visited : Bounded_Stack.Stack;
      Found   : Boolean;
      Result  : Value_Type;
   begin
      Find (Start_Node, Key, Found, Visited);
      if Found then
         Result := Tree_With_State.Value (Bounded_Stack.Top (Visited));
      else
         Result := Null_Value;
      end if;
      return Result;
   end Value_From;

   -----------------------
   -- Insert_Value_From --
   -----------------------

   procedure Insert_Value_From (Start_Node      : in out Tree_Node;
                                Key             : Key_Type;
                                Insert_Value    : Value_Type;
                                Overwrite       : Boolean;
                                Inserted        : out Boolean;
                                Insertion_Point : out Tree_Node;
                                Value_At_Node   : out Value_Type)
   is
      Visited        : Bounded_Stack.Stack;
      Key_Found      : Boolean;
      Is_Right       : Boolean;
       --  A Child of the Insertion_Point Node.
      Child          : Tree_Node;
      Subroot        : Tree_Node;
   begin
      if Start_Node = Empty_Node then
         --  First node of tree - Enter a new node with level 1 into the store
         Inserted := True;
         Tree_With_State.Add_Node
           (N       => Insertion_Point,
            The_Key => Key);
         Tree_With_State.Set_Value (Insertion_Point, Insert_Value);
         --  The new node is the root of the new tree
         Value_At_Node := Insert_Value;
      else
         --  Make sure that the tree does not already include the key.
         Find (Start_Node, Key, Key_Found, Visited);
         Insertion_Point := Bounded_Stack.Top (Visited);
         --  The Tree_Node at the top of the stack either contains the Key
         --  or is where a node with the Key will be inserted.
         if Key_Found and not Overwrite then
            --  The Key is already in the tree, do not add it again.
            Inserted := False;
            --  The node with the key is on the top of the visited stack.
            --  Get its value.
            Value_At_Node := Tree_With_State.Value (Insertion_Point);
         else
            Inserted := True;
            Value_At_Node := Insert_Value;

            if not Key_Found then
               --  A right branch if the value of Key is greater (or equal)
               --  to the Top Key, otherwise take the left branch.
               Is_Right := Tree_With_State.Key (Insertion_Point) < Key;

               --  Add a new child node to extend the tree
               Tree_With_State.Add_Node
                 (N       => Child,
                  The_Key => Key);
               Set_Branch (Is_Right   => Is_Right,
                           Node       => Insertion_Point,
                           Set_Node   => Child);
               Insertion_Point := Child;
               Subroot := Start_Node;
               Rebalance (Subroot, Visited);
            end if;
         end if;
      end if;
   end Insert_Value_From;

   function New_Enumerator_From (Start_Node : Valid_Tree_Node)
                                 return Enumerator
   is
      S : Bounded_Stack.Stack;
   begin
      Bounded_Stack.New_Stack (S);
      return (Enumerator'(Root    => Tree_With_State.Root,
                          Visited => S));
   end New_Enumerator_From;

   --  Visible Interface

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Boolean is (Tree_With_State.Is_Empty_Tree);

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth return Natural is
     (Tree_With_State.Level (Tree_With_State.Root));


   ----------------
   -- Is_Present --
   ----------------

   function Is_Present (Key : Key_Type) return Boolean is
      (Is_Present_From (Tree_With_State.Root, Key));

   -----------
   -- Value --
   -----------

   function Value (Key : Key_Type) return Value_Type is
     (Value_From (Tree_With_State.Root, Key));

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Tree_With_State.Init;
   end Init;

   ------------
   -- Insert --
   ------------

   procedure Insert (Key : Key_Type; Inserted : out Boolean) is
      Current_Root    : Tree_Node := Tree_With_State.Root;
      Insertion_Point : Valid_Tree_Node;
      Value_At_Node   : Value_Type;
   begin
      Insert_Value_From
        (Start_Node      => Current_Root,
         Key             => Key,
         Insert_Value    => Null_Value,
         Overwrite       => False,
         Inserted        => Inserted,
         Insertion_Point => Insertion_Point,
         Value_At_Node   => Value_At_Node);
      Tree_With_State.Set_Root (Current_Root);
   end Insert;

   -----------------------
   -- Insert_With_Value --
   -----------------------

   procedure Insert_With_Value
     (Key           : Key_Type;
      Insert_Value  : Value_Type; Inserted : out Boolean;
      Value_At_Node : out Value_Type)
   is
      Current_Root    : Tree_Node := Tree_With_State.Root;
      Insertion_Point : Valid_Tree_Node;
   begin
      Insert_Value_From
        (Start_Node      => Current_Root,
         Key             => Key,
         Insert_Value    => Insert_Value,
         Overwrite       => False,
         Inserted        => Inserted,
         Insertion_Point => Insertion_Point,
         Value_At_Node   => Value_At_Node);
      Tree_With_State.Set_Root (Current_Root);
   end Insert_With_Value;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value (Key : Key_Type; New_Value : Value_Type) is
      Current_Root    : Tree_Node := Tree_With_State.Root;
      Insertion_Point : Valid_Tree_Node;
      Value_At_Node   : Value_Type;
      Inserted        : Boolean;
   begin
      Insert_Value_From
        (Start_Node      => Current_Root,
         Key             => Key,
         Insert_Value    => New_Value,
         Overwrite       => True,
         Inserted        => Inserted,
         Insertion_Point => Insertion_Point,
         Value_At_Node   => Value_At_Node);
      Tree_With_State.Set_Root (Current_Root);
   end Update_Value;

   ---------------
   -- Last_Node --
   ---------------

   function Last_Node return Tree_Node is (Tree_With_State.Last_Node);

   ---------------------------
   -- Clear_Tree_Below_Node --
   ---------------------------

   procedure Clear_Tree_Below_Node (Node : Valid_Tree_Node) is
   begin
      Tree_With_State.Clear_Tree_Below_Node (Node);
   end Clear_Tree_Below_Node;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator return Enumerator is
      S : Bounded_Stack.Stack;
   begin
      Bounded_Stack.New_Stack (S);
      return (Enumerator'(Visited => S));
   end New_Enumerator;

   --------------
   -- Next_Key --
   --------------

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type) is
      Node : Tree_Node;
   begin
      Next_Node (E, Node);
      if Node /= Empty_Node then
         Key := Tree_With_State.Key (Node);
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
      Node : Tree_Node;
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


end Atrees_With_State;
