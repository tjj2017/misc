package body SPARK_2014.Trees
is
   ---------------
   -- Is_A_Node --
   ---------------

   function Is_A_Node (N : Tree_Node) return Boolean is (N > Empty_Node);

   ------------
   -- In_Tree --
   -------------

   function In_Tree (T : Tree_Type; N : Tree_Node) return Boolean is
     (N > Empty_Node and then N <= Dynamic_Tables.Last_Index (T.The_Tree));
   pragma Inline (In_Tree);

   ---------------------
   -- Node_Is_Present --
   ---------------------

   function Node_Is_Present (T : Tree_Type; N : Tree_Node) return Boolean is
   begin
      return In_Tree (T, N);
   end Node_is_Present;
   pragma Inline (Node_Is_Present);

   --------------
   -- Persists --
   --------------

   function Persists (T_Pre, T_Post : Tree_Type) return Boolean is
     (for all N in Tree_Node =>
        (if In_Tree (T_Pre, N) then In_Tree (T_Post, N)));

   --------------
   -- New_Tree --
   --------------

   procedure New_Tree (T : out Tree_Type) is
   begin
      Dynamic_Tables.Init (T.The_Tree);
   end New_Tree;

   -----------
   -- Level --
   -----------

   function Level (T : Tree_Type; N : Tree_Node) return Natural is
   begin
      return Dynamic_Tables.Get_Item (T.The_Tree, N).Level;
   end Level;
   pragma Inline (Level);

   ----------
   -- Left --
   ----------

   function Left (T : Tree_Type; N : Tree_Node) return Tree_Node is
      L : Tree_Node;
   begin
      L := Dynamic_Tables.Get_Item (T.The_Tree, N).Left;
      --# accept W, 444, "The left branch is either empty or In_Tree";
      --# assume L > Empty_Node -> In_Tree (T, L);
      pragma Assume ((if L > Empty_Node then In_Tree (T, L)),
                    "The left branch is either empty or In_Tree");
      return L;
   end Left;
   pragma Inline (Left);

   -----------
   -- Right --
   -----------

   function Right (T : Tree_Type; N : Tree_Node) return Tree_Node
   is
      R : Tree_Node;
   begin
      R := Dynamic_Tables.Get_Item (T.The_Tree, N).Right;
      --# accept W, 444, "The right branch is either empty or In_Tree";
      --# assume R > Empty_Node -> In_Tree (T, R);
      pragma Assume ((if R > Empty_Node then In_Tree (T, R)),
                    "The right branch is either empty or In_Tree");
      return R;
   end Right;
   pragma Inline (Right);

   ---------
   -- Key --
   ---------

   function Key (T : Tree_Type; N : Tree_Node) return Key_Type is
   begin
      return Dynamic_Tables.Get_Item (T.The_Tree, N).Key;
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (T : Tree_Type; N : Tree_Node) return Value_Type
   is
   begin
      return Dynamic_Tables.Get_Item (T.The_Tree, N).Value;
   end Value;
   pragma Inline (Value);

   --------------------
   -- Key_Is_Present --
   --------------------

   function Key_Is_Present (T : Tree_Type; K : Key_Type) return Boolean is
      (for some N in Tree_Node => In_Tree (T, N) and then Key (T, N) = K);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (T : in out Tree_Type;
                        N : Tree_Node;
                        Node_Level : Natural)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := T
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Level := Node_Level;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, T),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Level;
   pragma Inline (Set_Level);

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (T : in out Tree_Type;
                       N : Tree_Node;
                       Branch : Tree_Node)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := T
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Left := Branch;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, T),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Left;
   pragma Inline (Set_Left);

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (T : in out Tree_Type;
                        N : Tree_Node;
                        Branch : Tree_Node)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := T
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Right := Branch;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, T),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (T : in out Tree_Type; N : Tree_Node;
                      The_Key : Key_Type)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := T
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Key := The_Key;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, T),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Key;
   pragma Inline (Set_Key);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (T : in out Tree_Type;
                        N : Tree_Node;
                        Node_Value : Value_Type)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := T
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Value := Node_Value;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, T),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Value;
   pragma Inline (Set_Value);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (T : in out Tree_Type;
                       N : out Tree_Node;
                       The_Key : Key_Type)
   is
      Node : Actual_Node;
      T_Entry : constant Tree_Type := T
        with Ghost;
   begin
      Node := Actual_Node'
        (Key   => The_Key,
         Value => Null_Value,
         Level => 1,
         Left  => Empty_Node,
         Right => Empty_Node);
      Dynamic_Tables.Append (T.The_Tree, Node);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, T),
                     "Setting a component of a tree does not remove Nodes");
      N := Dynamic_Tables.Last_Index (T.The_Tree);
      pragma Assert (Key (T, N) = The_Key);
   end Add_Node;
   pragma Inline (Add_Node);

   -----------
   -- Clear --
   -----------

   procedure Clear (T : in out Tree_Type; N : Tree_Node)
   is
      New_Last : Tree_Node;
   begin
      if N /= Empty_Node then
         New_Last := N - 1;
      else
         New_Last := N;
      end if;

      Dynamic_Tables.Set_Last
        (T       => T.The_Tree,
         New_Val => New_Last);
   end Clear;

end SPARK_2014.Trees;
