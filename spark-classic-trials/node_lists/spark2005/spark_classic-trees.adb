package body SPARK_Classic.Trees is

   -- In_Tree --
   -------------

   function In_Tree (T : Tree_Type; N : Tree_Node) return Boolean
   --# return  N > Empty_Node and N <= Dynamic_Tables.Last_Index (T.The_Tree);
   is
      Result : Boolean;
   begin
      Result :=  N > Empty_Node and then
        N <= Dynamic_Tables.Last_Index (T.The_Tree);
      return Result;
   end In_Tree;
   pragma Inline (In_Tree);

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

   function Left (T : Tree_Type; N : Tree_Node) return Tree_Node
   is
      L : Tree_Node;
   begin
      L := Dynamic_Tables.Get_Item (T.The_Tree, N).Left;
      --# accept W, 444, "The left branch is either empty or In_Tree";
      --# assume L > Empty_Node -> In_Tree (T, L);
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

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (T : in out Tree_Type;
                        N : Tree_Node;
                        Node_Level : Natural)
   is
      Node_Contents : Actual_Node;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Level := Node_Level;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
      --# assume Persists (T~, T);
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
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Left := Branch;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
      --# assume Persists (T~, T);
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
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Right := Branch;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
      --# assume Persists (T~, T);
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (T : in out Tree_Type; N : Tree_Node;
                      The_Key : Key_Type)
   is
      Node_Contents : Actual_Node;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Key := The_Key;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
      --# assume Persists (T~, T);
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
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Value := Node_Value;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
      --# assume Persists (T~, T);
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
   begin
      Node := Actual_Node'
        (Key   => The_Key,
         Value => Null_Value,
         Level => 1,
         Left  => Empty_Node,
         Right => Empty_Node);
      Dynamic_Tables.Append (T.The_Tree, Node);
      N := Dynamic_Tables.Last_Index (T.The_Tree);
      --# assume Persists (T~, T);
      --# accept W, 444, "Appending a node adds it into the tree";
      --# accept W, 444, "N is the position in the tree of the Actual_Node";
      --# assume Left (T, N) = Empty_Node and
      --#        Right (T, N) = Empty_Node and
      --#        Value (T, N) = Null_Value and
      --#        Key (T, N) = The_Key;
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

      --# check Dynamic_Tables.Last_Index (T.The_Tree) = New_Last;
      --# check N = Empty_Node or N > New_Last;
      --# accept W, 444, "If N = Empty_Node or N > New_Last by definition ",
      --#                "N is not in the tree";
      --# assume not In_Tree (T, N);
   end Clear;

end SPARK_Classic.Trees;
