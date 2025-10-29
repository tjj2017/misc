package body Basic_Tree is

   ----------
   -- Init --
   ----------

   procedure Init (T : out Tree) is
      Temp_Instance : Table.Instance;
   begin
      Table.Init (Temp_Instance);
      T := Tree (Temp_Instance);
   end Init;

    -----------
   -- Level --
   -----------

   function Level (T : Tree; I : Valid_Node_Index) return Level_Type is
     (T.Table (I).Level);

   ----------
   -- Left --
   ----------

   function Left (T : Tree; I : Valid_Node_Index) return Valid_Node_Index is
     (T.Table (I).Left);

   -----------
   -- Right --
   -----------

   function Right (T: Tree; I : Valid_Node_Index) return Valid_Node_Index is
     (T.Table (I).Right);

   ---------
   -- Key --
   ---------

   function Key (T : Tree; I : Valid_Node_Index) return Key_Type is
     (T.Table (I).Key);

   -----------
   -- Value --
   -----------

   function Value (T : Tree; I : Valid_Node_Index) return Value_Type is
     (T.Table (I).Value);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (T: in out Tree;
                        I : Valid_Node_Index;
                        Node_Level : Level_Type)
   is
   begin
      T.Table (I).Level := Node_Level;
   end Set_Level;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (T : in out Tree;
                       I : Valid_Node_Index;
                       Branch : Valid_Node_Index)
   is
   begin
      T.Table (I).Left := Branch;
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (T : in out Tree;
                        I : Valid_Node_Index;
                        Branch : Valid_Node_Index)
   is
   begin
      T.Table (I).Right := Branch;
   end Set_Right;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (T : in out Tree;
                      I : Valid_Node_Index;
                      The_Key : Key_Type)
   is
   begin
      T.Table (I).Key := The_Key;
   end Set_Key;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (T : in out Tree;
                        I : Valid_Node_Index;
                        Node_Value : Value_Type) is
   begin
      T.Table (I).Value := Node_Value;
   end Set_Value;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (T         : in out Tree;
                       New_Index : out Valid_Node_Index;
                       The_Key   : Key_Type)
   is
   begin
      Table.Append (Table.Instance (T), Null_Actual_Node);
      New_Index := Table.Last (Table.Instance (T));
      T.Table (New_Index).Key := The_Key;
   end Add_Node;

   ---------------------
   -- Last_Node_Index --
   ---------------------

   function Last_Node_Index (T : Tree) return Node_Index is
     (Table.Last (Table.Instance (T)));

   ---------------------------
   -- Clear_Tree_Below_Node --
   --------------------------

   procedure Clear_Tree_Below_Node (T : in out Tree;
                                    Final_Node : Valid_Node_Index)
   is
   begin
      Table.Set_Last (Table.Instance (T), Final_Node);
      T.Table (Final_Node).Left := Empty_Node;
      T.Table (Final_Node).Right := Empty_Node;
    end Clear_Tree_Below_Node;

end Basic_Tree;
