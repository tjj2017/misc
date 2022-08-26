pragma Ada_2012;
package body SPARK_Classic.Trees is

   ----------
   -- Tree --
   ----------

   package body Tree is

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid (N : Tree_Node) return Boolean is
      begin
        return  N /= Empty_Node;
      end Is_Valid;
      pragma Inline (Is_Valid);

      --------------
      -- Present  --
      --------------

      function Present (N : Tree_Node) return Boolean is
      begin
         return N /= Empty_Node;
      end Present;
      pragma Inline (Present);


      --------------
      -- New_Tree --
      --------------

      function New_Tree return Tree_Type is
         Result : Tree_Type;
      begin
         Dynamic_Tree.Init (Result.The_Tree);
         Result.T_Node := Empty_Node;
         return Result;
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

      function Left
        (T : Tree_Type;
         N : Tree_Node)
         return Tree_Node
      is
      begin
         return T.The_Tree.Table (N).Left;
      end Left;
      pragma Inline (Left);

      -----------
      -- Right --
      -----------

      function Right
        (T : Tree_Type;
         N : Tree_Node)
         return Tree_Node
      is
      begin
         return T.The_Tree.Table (N).Right;
      end Right;
      pragma Inline (Right);

      -----------
      -- Value --
      -----------

      function Value
        (T : Tree_Type;
         N : Tree_Node)
         return Element_Type
      is
      begin
         return T.The_Tree.Table (N).Value;
      end Value;

      ---------------
      -- Set_Level --
      ---------------

      procedure Set_Level
        (T : in out Tree_Type;
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

      procedure Set_Left
        (T : in out Tree_Type;
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

      procedure Set_Right
        (T : in out Tree_Type;
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

      procedure Set_Value
        (T : in out Tree_Type;
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

      procedure Add_Node
        (T : in out Tree_Type;
         N : out Tree_Node;
         V : Element_Type)
      is
         Node : constant Actual_Node := Actual_Node'
           (Value => V,
            Level => 0,
            Left  => Empty_Node,
            Right => Empty_Node);
      begin
         Dynamic_Tree.Append (T.The_Tree, Node);
         N := Dynamic_Tree.Last (T.The_Tree);
       end Add_Node;

      -----------
      -- Clear --
      -----------

      procedure Clear (T : in out Tree_Type; N : Tree_Node) is
      begin
         Dynamic_Tree.Set_Last
           (T       => T.The_Tree,
            New_Val => N);
      end Clear;

   end Tree;

end SPARK_Classic.Trees;
