package body SPARK_Classic.Symbols.Nodes
with Refined_State => (Store => (Tree_Store, In_Progress))
   --  --# own Store is Tree_Store, In_Progress;
is
   type Stack is array (Positive range <>) of Node_Tree.Tree_Node;

   Tree_Store  : Node_Tree.Tree_Type;
   In_Progress : Boolean;

   Take_Left  : constant Boolean := False;
   Take_Right : constant Boolean := True;

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
      Tree_Store.Add_Node (The_Node, N);
   end New_Node;

   function Get_Child (Is_Right : Boolean; Node : A_Tree.Tree_Node)
                       return A_Tree.Tree_Node
   with Global => Tree_Store;

   function Get_Child (Is_Right : Boolean; Node : A_Tree.Tree_Node)
                       return A_Tree.Tree_Node
   --  --# global Tree_Store;
   is
      Result : A_Tree.Tree_Node;
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
                         Node     : A_Tree.Tree_Node;
                         New_Node : A_Tree.Tree_Node)
   with Global => (In_Out => Tree_Store);

   procedure Set_Branch (Is_Right : Boolean;
                         Node     : A_Tree.Tree_Node;
                         New_Node : A_Tree.Tree_Node)
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

   procedure Skew (Root : in out A_Tree.Tree_Node)
     with Global => (In_Out => Tree_Store);

   procedure Skew (Root : in out A_Tree.Tree_Node)
   --  --# global in out Tree_Store;
   is
      Left_Child : constant Node_Tree.Tree_Node := Tree_Store.Left (Root);
   begin
      --  No action is performed if the levels of the root and left nodes
      --  are not equal.
      if Tree_Store.Level (Left_Child) = Tree_Store.Level (Root) then
         --  The left child has the same level as its parent breaking
         --  rule 2 of an Anderson tree. To resolve rotate right at the parent.
         --  That is, the root node left child becomes the right child of
         --  root node left node.  The right child of the root node left child
         --  becomes the root index, and lastly, the index of the root
         --  left child becomes the new root {index}.
         Tree_Store.Set_Left
           (N      => Root,
            Branch => Tree_Store.Right (Left_Child));
         A_Tree.Set_Right
           (T      => Tree_Store,
            N      => Left_Child,
            Branch => Root);
         --  The root now becomes the left child.
         Root := Left_Child;
      end if;
   end Skew;

   procedure Split (Root : in out A_Tree.Tree_Node)
     with Global => (In_Out => Tree_Store);

   procedure Split (Root : in out A_Tree.Tree_Node)
    --  --# global in out Tree_Store;
   is
      Right_Child : constant A_Tree.Tree_Node :=
        A_Tree.Right (Tree_Store, Root);
      Right_Right_Child : A_Tree.Tree_Node :=
        A_Tree.Right (Tree_Store, Right_Child);
   begin
      --  No action is taken if there are not two consecutive right children
      -- with the same level
      if A_Tree.Level (Tree_Store, Right_Right_Child) =
        A_Tree.Level (Tree_Store, Root)
      then
         --  There are two consecutive right children with the same level
         --  Breaking rule 3 of an Anderson tree.
         --  To resolve rotate left and increment the level of the parent.
         --  That is, the ruight child of the root becomes the left child
         --  of the right child of the root. The right child of the
         --  right child of the root becomes the root
         --  the right child of the root becomes the new root and its level
         --  is incremented.
         A_Tree.Set_Right
           (T      => Tree_Store,
            N      => Root,
            Branch => A_Tree.Left (Tree_Store, Right_Child));
         A_Tree.Set_Left
           (T      => Tree_Store,
            N      => Right_Child,
            Branch => Root);
         --  The root noe becomes the right child.
         Root := Right_Child;
         --  Increment the level of the new root.
         A_Tree.Set_Level (Tree_Store, Root,
                           A_Tree.Level (Tree_Store, Root) + 1);
      end if;
   end Split;


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

   --------------
   -- New_List --
   --------------

   procedure New_List (List : out Node_List)
   is
   begin
      In_Progress := True;
      List.Root := A_Tree.Empty_Node;
   end New_List;

   ------------
   -- Insert --
   ------------

   procedure Insert (N : Types.Node_Id; List : in out Node_List)
   is
      New_Node : A_Tree.Tree_Node;
   begin
      if List.Root = A_Tree.Empty_Node then
         --  First node of list - Enter a new node with level 1 into the store
         A_Tree.Add_Node
           (T => Tree_Store,
            N => New_Node,
            V => N);
         --  The new node is the root of the new list
         List.Root := New_Node;
      else
         Do_Insert : declare
            Root_Level    : constant Natural :=
              A_Tree.Level (Tree_Store, List.Root);
            --  The Current_Root is initially set to the root of the tree.
            Current_Root  : A_Tree.Tree_Node := List.Root;
            --  A stack to record visited nodes.
            --  A adding a new node does not count as visiting that node.
            --  A new node will always be leaf and does not need to be
            --  processed when working back up the tree.
            --  Hence, a stack depth equal to the Current_Root leve should
            --  always be sufficient.
            Visited       : Stack (1 .. Root_Level);
            Top           : Natural := 0;
            Top_Node      : A_Tree.Tree_Node;
            --  Direction: Left = False, Right = True
            Dir           : Boolean;

            --  A Child index of the current node.
            Child         : A_Tree.Tree_Node;
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
               Dir := A_Tree.Value (Tree_Store, Current_Root) < N;
               Child := Get_Child (Dir, Current_Root);

               exit when Child = A_Tree.Empty_Node;

               --  Traverse the tree: the Current_Root is set to one of its
               --  children.
               Current_Root := Child;
            end loop;

            --  A leaf node has been reached in the search.
            --  Add a new child node to extend the tree
            A_Tree.Add_Node
              (T => Tree_Store,
               N => Child,
               V => N);
            Set_Branch (Is_Right => Dir,
                        Node     => Current_Root,
                        New_Node => Child);

            --  Now rebalance the tree by working back up through the visited
            --  node indices on the stack, Up.
            for Stack_Top in reverse Natural range 1 .. Top loop
               --  Make the Current_Root equal to the the index at the top of
               --  the stack and the Current_Node equal to the node selected
               --  by the Current_Root.
               Top_Node := Visited (Stack_Top);
               Current_Root := Top_Node;

               --  Perform the Anderson Tree Skew and Split operations on
               --  the Current_Root.  The node which is the Current_Root
               --  may be changed by Skew and Split and so the Index contained
               --  in Current_Root may change.
               Skew (Current_Root);
               Split (Current_Root);

               --  Check if the value of the Current_Root has changed.
               if Current_Root /= Top_Node and then Stack_Top /= 1 then
                  --  The value of the Current_Root
                  --  has changed and the stack will have a
                  --  direct ancestor of the Current_Root in Visited (Top - 1).
                  --  The branch that has the Current_Root as its child
                  --  has to be patched up to contain the new value of
                  --  Current_Root as its child.
                  --  As the value of the Current_Root may have changed
                  Patch_Ancestor :
                  declare
                     Direct_Ancestor  : constant A_Tree.Tree_Node :=
                       Visited (Stack_Top - 1);
                  begin
                     --  This boolean expression determines which branch of
                     --  the direct ancestor has the original root index
                     --  as its child.
                     --  False => Left, True => Right.
                     Dir :=
                       A_Tree.Left (Tree_Store, Direct_Ancestor) = Top_Node;
                     Set_Branch
                       (Is_Right => Dir,
                        Node     => Direct_Ancestor,
                        New_Node => Current_Root);
                  end Patch_Ancestor;
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

   function New_Enumerator (N_List : Node_List) return Enumerator is
   begin
      return Enumerator'
        (Root    => N_List,
         Place   => N_List.Root,
         Visited => Dynamic_Stack.New_Stack,
         Dir     => Left);
   end New_Enumerator;

   function Next (E : Enumerator) return Types.Node_Id is
      Result : Types.Node_Id;
   begin
      return Types.Empty;
   end Next;
--        if not Present (E.Place) then
--           Result := Types.Empty;
--        else
--           Traverse_Tree : declare
--              Current_Index : Table_Index;
--              Current_Node  : List_Store.Store_Node :=
--                List_Store.Item (Current_Index);
--           begin
--              if Dir = Right then
--                 if not A_Tree.Present (Current_Node.Right) then
--                    Result := Current_Node.Value;
--                    Dynamic_Stack.Pop (S.Index);
--                 else
--                    Dir := Left;
--                 end if;
--              end if;
--              --  First travese the tree down the left children unti a leaf
--              --  is found.
--             while Current_Node.Left /= No_Index loop
--                 Current_Node  :=
--                   List_Store.Current_Item (Current_Index);
--                 Dynamic_Stack.Push (Current_Index, E.Visited);
--                 Current_Index := Current_Node.Left;
--              end loop;
--              --  A leaf has been reached; The Current_Node contains
--              --  the required value.
--              Result := Current_Node.Value;
--              --  The Enumerator index for the next call of Next is now at the
--              --  top of the stack pop it off ready for the next call and
--              --  set the Enumerator direction to Right to process the
--              --  the right child (if it exists) of the current node.
--              Dynamic_Stack.Pop (E.Index, E.Visited);
--              E.Dir := Right;
--           end Traverse_Tree;
--        else
--           Right_Traversal : declare
--              Current_Index : Table_Index := E.Index;
--              Current_Node  : List_Store.Store_Node;
--           begin
--
--
--
--              Current_Index := Store_Node.Item (Current_Index).Right;
--
--              --  Next has to be primed for the next call.
--              while Current_Index = No_Index and then
--                not Dynamic_Stack.Is_Empty (E.Visited)
--              loop
--                 Current_Node :=
--              --  Traverse back up the tree until a node with a right child
--              --  is found.
--              --  If the right child of the Current_Node is not No_Index then
--              --  the index for the next call of Next is the right child
--              --  of the Current_Node otherwise
--
--
--  List_Store.Item (E.Index) = No_Index then
--           Result := List_Store.Item (E.Index);
--
--           if Dynamic_Stack.Is_Empty (E.Visited) then
--              Result := Types.Empty;
--           else
--              Dynamic_Stack.Pop (Pred, E.Visited)
--
--     -- --#  global List_Store.Store;
--     with Global => List_Store.Store;
end SPARK_Classic.Symbols.Nodes;

