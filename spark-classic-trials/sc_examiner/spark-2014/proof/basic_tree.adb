package body Basic_Tree is

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (T : Tree) return Node_Index is (T. Last);

   ----------
   -- Init --
   ----------

   procedure Init (T : out Tree) is
   begin
      T.Last := Null_Index;  -- Declaration of object of type T initializes the Object;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset (T : in out Tree) is
   begin
      T.Last := Null_Index;
   end Reset;

    -----------
   -- Level --
   -----------

   function Level (T : Tree; I : Valid_Node_Index) return Level_Type is
     (T.Table (I).Level);

   ----------
   -- Left --
   ----------

   function Left (T : Tree; I : Valid_Node_Index) return Node_Index is
     (T.Table (I).Left);

   -----------
   -- Right --
   -----------

   function Right (T: Tree; I : Valid_Node_Index) return Node_Index is
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
      T.Last := T.Last + 1;
      T.Table (T.Last) := Null_Actual_Node;
      New_Index := T.Last;
      T.Table (New_Index).Key := The_Key;
   end Add_Node;

   ---------------------
   -- Last_Node_Index --
   ---------------------

   function Last_Node_Index (T : Tree) return Valid_Node_Index is (T.Last);

   ---------------------------
   -- Clear_Tree_Below_Node --
   --------------------------

   procedure Clear_Tree_Below_Node (T : in out Tree;
                                    Final_Node : Valid_Node_Index)
   is
   begin
      T.Last := Final_Node;
    end Clear_Tree_Below_Node;

end Basic_Tree;
