package body SPARK_2014.Trees with
  Refined_State => (Tree_Store => Refined_Tree_Store)
is
   Refined_Tree_Store : Actual_Tree_Type;

   function TS return Tree_Type is (Tree_Type (Refined_Tree_Store))
     with Refined_Global => Refined_Tree_Store;

   --  Ghost functions which use the Ghost Type representing the type
   --  of the Abstract_State Tree_Store.

   function In_Tree_Ghost (T : Tree_Type; N : Tree_Node) return Boolean is
     (N > Empty_Node and then
      N <= Dynamic_Tables.Last_Index (T.The_Tree)) with Ghost;

   function Key_Ghost (T : Tree_Type; N : Tree_Node) return Key_Type is
     (Dynamic_Tables.Get_Item (T.The_Tree, N).Key) with Ghost;

   function Key_Is_Present_Ghost (T : Tree_Type; K : Key_Type) return Boolean
   is (for some N in Tree_Node => In_Tree_Ghost (T, N) and then
       Key_Ghost (T, N) = K) with Ghost;

   ---------------
   -- Is_A_Node --
   ---------------

   function Is_A_Node (N : Tree_Node) return Boolean is (N > Empty_Node);

   ------------
   -- In_Tree --
   -------------


   function In_Tree (N : Tree_Node) return Boolean is
     (N > Empty_Node and then
      N <= Dynamic_Tables.Last_Index (Refined_Tree_Store.The_Tree))
       with Refined_Global => Refined_Tree_Store;
   pragma Inline (In_Tree);

   --------------------
   -- Key_Is_Present --
   --------------------

   function Key_Is_Present (K : Key_Type) return Boolean is
     (for some N in Tree_Node => In_Tree (N) and then Key (N) = K) with
       Refined_Global => Refined_Tree_Store;

   ---------------------
   -- Node_Is_Present --
   ---------------------

   function Node_Is_Present (N : Tree_Node) return Boolean is (In_Tree (N))
     with Refined_Global => Refined_Tree_Store;
   pragma Inline (Node_Is_Present);

   --------------
   -- Persists --
   --------------

   function Persists (T_Pre, T_Post : Tree_Type) return Boolean with
     Refined_Global => Refined_Tree_Store
   is
   begin
     return (for all N in Tree_Node =>
           (if In_Tree_Ghost (T_Pre, N) then In_Tree_Ghost (T_Post, N))) and
        (for all K in Key_Type =>
             (if Key_Is_Present_Ghost (T_Pre, K) then
                   Key_Is_Present_Ghost (T_Post, K)));
   end Persists;

    -----------
   -- Level --
   -----------

   function Level (N : Tree_Node) return Natural with
     Refined_Global => Refined_Tree_Store
   is
   begin
      return (Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N).Level);
   end Level;
   pragma Inline (Level);

   ----------
   -- Left --
   ----------

   function Left (N : Tree_Node) return Tree_Node with
     Refined_Global => Refined_Tree_Store
   is
      L : Tree_Node;
   begin
      L := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N).Left;
      pragma Assume ((if L > Empty_Node then In_Tree (L)),
                    "The left branch is either empty or In_Tree");
      return L;
   end Left;
   pragma Inline (Left);

   -----------
   -- Right --
   -----------

   function Right (N : Tree_Node) return Tree_Node with
     Refined_Global => Refined_Tree_Store
   is
      R : Tree_Node;
   begin
      R := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N).Right;
      pragma Assume ((if R > Empty_Node then In_Tree (R)),
                    "The right branch is either empty or In_Tree");
      return R;
   end Right;
   pragma Inline (Right);

   ---------
   -- Key --
   ---------

   function Key (N : Tree_Node) return Key_Type with
     Refined_Global => Refined_Tree_Store
   is
   begin
     return (Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N).Key);
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (N : Tree_Node) return Value_Type with
     Refined_Global => Refined_Tree_Store
   is
   begin
     return (Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N).Value);
   end Value;
   pragma Inline (Value);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (N : Tree_Node;
                        Node_Level : Natural) with
     Refined_Global => (In_Out => Refined_Tree_Store)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N);
      Node_Contents.Level := Node_Level;
      Dynamic_Tables.Set_Item (Refined_Tree_Store.The_Tree, N, Node_Contents);

      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Level;
   pragma Inline (Set_Level);

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (N : Tree_Node;
                       Branch : Tree_Node) with
     Refined_Global => (In_Out => Refined_Tree_Store)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N);
      Node_Contents.Left := Branch;
      Dynamic_Tables.Set_Item (Refined_Tree_Store.The_Tree, N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Left;
   pragma Inline (Set_Left);

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (N : Tree_Node;
                        Branch : Tree_Node) with
     Refined_Global => (In_Out => Refined_Tree_Store)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N);
      Node_Contents.Right := Branch;
      Dynamic_Tables.Set_Item (Refined_Tree_Store.The_Tree, N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (N : Tree_Node; The_Key : Key_Type) with
     Refined_Global => (In_Out => Refined_Tree_Store)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N);
      Node_Contents.Key := The_Key;
      Dynamic_Tables.Set_Item (Refined_Tree_Store.The_Tree, N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (Key_Is_Present (The_Key),
                     "The key has just been set.");
   end Set_Key;
   pragma Inline (Set_Key);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (N : Tree_Node;
                        Node_Value : Value_Type) with
     Refined_Global => (In_Out => Refined_Tree_Store)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (Refined_Tree_Store.The_Tree, N);
      Node_Contents.Value := Node_Value;
      Dynamic_Tables.Set_Item (Refined_Tree_Store.The_Tree, N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
   end Set_Value;
   pragma Inline (Set_Value);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (N : out Tree_Node;
                       The_Key : Key_Type) with
     Refined_Global => (In_Out => Refined_Tree_Store)
   is
      Node : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node := Actual_Node'
        (Key   => The_Key,
         Value => Null_Value,
         Level => 1,
         Left  => Empty_Node,
         Right => Empty_Node);
      Dynamic_Tables.Append (Refined_Tree_Store.The_Tree, Node);
      pragma Assume (Persists (T_Entry, TS),
                     "Adding a Node to a tree does not remove Nodes");
      N := Dynamic_Tables.Last_Index (Refined_Tree_Store.The_Tree);
      pragma Assume (Key (N) = The_Key,
                    "A node with a Key = The Key has been created by Append");
   end Add_Node;
   pragma Inline (Add_Node);

   -----------
   -- Clear --
   -----------

   procedure Clear (N : Tree_Node) with
   Refined_Global => (In_Out => Refined_Tree_Store)
   is
      New_Last : Tree_Node;
   begin
      if N /= Empty_Node then
         New_Last := N - 1;
      else
         New_Last := N;
      end if;

      Dynamic_Tables.Set_Last
        (T       => Refined_Tree_Store.The_Tree,
         New_Val => New_Last);
   end Clear;

end SPARK_2014.Trees;
