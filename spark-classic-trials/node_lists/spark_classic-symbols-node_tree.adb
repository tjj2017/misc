package body SPARK_Classic.Symbols.Node_Tree is

   -------------
   -- In_Tree --
   -------------

   function In_Tree (T : Tree_Type; N : Tree_Node) return Boolean is
   begin
      return  N /= Empty_Node and then N <=  Dynamic_Tree.Last (T.The_Tree);
   end In_Tree;
   pragma Inline (In_Tree);

   -------------
   -- Present --
   -------------

   function Present (T : Tree_Type; N : Tree_Node) return Boolean is
   begin
      return In_Tree (T, N);
   end Present;
   pragma Inline (Present);

   --------------
   -- New_Tree --
   --------------

   procedure New_Tree (T : out Tree_Type) is
   begin
      Dynamic_Tree.Init (T.The_Tree);
   end New_Tree;

   -----------
   -- Level --
   -----------

   function Level (T : Tree_Type; N : Tree_Node) return Natural is
   begin
      return T.The_Tree.Table (N).Level;
   end Level;
   pragma Inline (Level);

   ----------
   -- Left --
   ----------

   function Left (T : Tree_Type; N : Tree_Node) return Tree_Node
   is
   begin
      return T.The_Tree.Table (N).Left;
   end Left;
   pragma Inline (Left);

   -----------
   -- Right --
   -----------

   function Right (T : Tree_Type; N : Tree_Node) return Tree_Node
   is
   begin
      return T.The_Tree.Table (N).Right;
   end Right;
   pragma Inline (Right);

   ---------
   -- Key --
   ---------

   function Key (T : Tree_Type; N : Tree_Node) return Key_Type
   is
   begin
      return T.The_Tree.Table (N).Value.Key;
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (T : Tree_Type; N : Tree_Node) return Element_Type
   is
   begin
      return T.The_Tree.Table (N).Value;
   end Value;
   pragma Inline (Value);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (T : in out Tree_Type;
                        N : Tree_Node;
                        Node_Level : Natural)
   is
   begin
      T.The_Tree.Table (N).Level := Node_Level;
   end Set_Level;
   pragma Inline (Set_Level);

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (T : in out Tree_Type;
                       N : Tree_Node;
                       Branch : Tree_Node)
   is
   begin
      T.The_Tree.Table (N).Left := Branch;
   end Set_Left;
   pragma Inline (Set_Left);

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (T : in out Tree_Type;
                        N : Tree_Node;
                        Branch : Tree_Node)
   is
   begin
      T.The_Tree.Table (N).Right := Branch;
   end Set_Right;
   pragma Inline (Set_Right);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (T : in out Tree_Type;
                        N : Tree_Node;
                        Node_Value : Element_Type)
   is
   begin
      T.The_Tree.Table (N).Value := Node_Value;
   end Set_Value;
   pragma Inline (Set_Value);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (T : in out Tree_Type;
                       N : out Tree_Node;
                       V : Element_Type)
   is
      Node : constant Actual_Node := Actual_Node'
        (Value => V,
         Level => 1,
         Left  => Empty_Node,
         Right => Empty_Node);
   begin
      Dynamic_Tree.Append (T.The_Tree, Node);
      N := Dynamic_Tree.Last (T.The_Tree);
   end Add_Node;
   pragma Inline (Add_Node);

   -----------
   -- Clear --
   -----------

   procedure Clear (T : in out Tree_Type; N : Tree_Node) is
   begin
      if In_Tree (T, N) then
         Dynamic_Tree.Set_Last
           (T       => T.The_Tree,
            New_Val => N - 1);
      end if;
   end Clear;

end SPARK_Classic.Symbols.Node_Tree;
