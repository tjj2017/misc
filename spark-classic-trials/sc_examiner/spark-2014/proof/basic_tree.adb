package body Basic_Tree with
SPARK_Mode
is

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (T : Tree) return Node_Index is (T. Last);

   ----------
   -- Init --
   ----------

   procedure Init (T : out Tree) is
   begin
      T.Table := (others => Null_Actual_Node);
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

   function Left (T : Tree; I : Valid_Node_Index) return Node_Index with
     Refined_Post => Left'Result = T.Table (I).Left and
                     (if Left'Result /= Null_Index then
                      In_Tree (T, Left'Result))
   is
      Left_Index : Node_Index := T.Table (I).Left;
   begin
      pragma Assume ((if Left_Index /= Null_Index then In_Tree (T, Left_Index)),
                     "The only published mechanism for adding a node to the " &
                       "tree, T, by a call to Add_Node. " &
                       "Add_Node sets the left and right branches of each " &
                       "node when added to Null_Index_" &
                       "The left and right branches of a node are only set " &
                       "by calling Set_Left, or Set_Right.  Both of these " &
                       "subprograms ensure the left and right branches " &
                       "of a node can only be set to the a valid node index " &
                       "of the tree, T.");
      return Left_Index;
   end Left;

   -----------
   -- Right --
   -----------

   function Right (T: Tree; I : Valid_Node_Index) return Node_Index with
     Refined_Post => Right'Result = T.Table (I).Right and
                     (if Right'Result /= Null_Index then
                      In_Tree (T, Right'Result))
   is
      Right_Index : Node_Index := T.Table (I).Right;
   begin
      pragma Assume ((if Right_Index /= Null_Index then In_Tree (T, Right_Index)),
                     "The only published mechanism for adding a node to the " &
                       "tree, T, by a call to Add_Node. " &
                       "Add_Node sets the left and right branches of each " &
                       "node when added to Null_Index_" &
                       "The left and right branches of a node are only set " &
                       "by calling Set_Left, or Set_Right.  Both of these " &
                       "subprograms ensure the left and right branches " &
                       "of a node can only be set to the a valid node index " &
                       "of the tree, T.");
      return Right_Index;
   end Right;

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
      T_In : constant Tree := T with
      Ghost;
   begin
      T.Table (I).Level := Node_Level;
      pragma Assume (Contents_Preserved (T, T_In),
                     "Updating the Level of a node does not affect the "&
                       "contents of the tree, T.");
      pragma Assume (Structure_Preserved_Except (T, T_In, I),
                     "Updating the Level of a node only changes that node " &
                       "All other nodes of T are unaffected.");
      pragma Assume (Left (T, I) = Left (T_In, I) and
                       Right (T, I) = Right (T_In, I),
                     "Updating the Level of a node does not affect the " &
                       "Left or Right branches of the node.");
   end Set_Level;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (T : in out Tree;
                       I : Valid_Node_Index;
                       Branch : Valid_Node_Index)
   is
      T_In : constant Tree := T with
      Ghost;
   begin
      T.Table (I).Left := Branch;
      pragma Assert (Left (T, I) = Branch);
      pragma Assume (Contents_Preserved (T, T_In),
                     "Updating the Left branch of a node does not affect the "&
                       "contents of the tree, T.");
      pragma Assume (Structure_Preserved_Except (T, T_In, I),
                     "Updating the Left branch of a node only changes that " &
                       " node. All other nodes of T are unaffected.");
      pragma Assume (Right (T, I) =  Right (T_In, I) and
                       Level (T, I) = Level (T_In, I),
                     "Updating the Left branch of a node does not affect the " &
                       "Right branch or the Level of the node.");
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (T : in out Tree;
                        I : Valid_Node_Index;
                        Branch : Valid_Node_Index)
   is
      T_In : constant Tree := T with
      Ghost;
   begin
      T.Table (I).Right := Branch;
      pragma Assert (Right (T, I) = Branch);
      pragma Assume (Contents_Preserved (T, T_In),
                     "Updating the Right branch of a node does not affect the "&
                       "contents of the tree, T.");
      pragma Assume (Structure_Preserved_Except (T, T_In, I),
                     "Updating the Right branch of a node only changes that " &
                       "node. All other nodes of T are unaffected.");
      pragma Assume (Left (T, I) =  Left (T_In, I) and
                       Level (T, I) = Level (T_In, I),
                     "Updating the Right branch of a node does not affect the " &
                       "Left branch or the Level of the node.");
   end Set_Right;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (T : in out Tree;
                      I : Valid_Node_Index;
                      The_Key : Key_Type)
   is
      T_In : constant Tree := T with
      Ghost;
   begin
      T.Table (I).Key := The_Key;
      pragma Assume (Structure_Preserved (T, T_In),
                     "Updating the Key of a node does not affect the "&
                       "structure of the tree, T.");
      pragma Assume (Contents_Preserved_Except (T, T_In, I),
                     "Updating the Key of a node only changes that " &
                       "node. All other nodes of T are unaffected.");
      pragma Assume (Value (T, I) = Value (T_In, I),
                     "Updating the Key of a node does not affect the " &
                       "Value of the node.");
   end Set_Key;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (T : in out Tree;
                        I : Valid_Node_Index;
                        Node_Value : Value_Type)
   is
      T_In : constant Tree := T with
      Ghost;
   begin
      T.Table (I).Value := Node_Value;
      pragma Assume (Structure_Preserved (T, T_In),
                     "Updating the Value of a node does not affect the "&
                       "structure of the tree, T.");
      pragma Assume (Contents_Preserved_Except (T, T_In, I),
                     "Updating the Value of a node only changes that " &
                       "node. All other nodes of T are unaffected.");
      pragma Assume (Key (T, I) = Key (T_In, I),
                     "Updating the Value of a node does not affect the " &
                       "Key of the node.");
   end Set_Value;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (T         : in out Tree;
                       New_Index : out Valid_Node_Index;
                       The_Key   : Key_Type)
   is
      Node : Actual_Node := Null_Actual_Node;
      T_In : constant Tree := T with Ghost;
   begin
      Node.Key := The_Key;
      pragma Assume
        (T.Last < Node_Index'Last,
         "Node_Index is defined to have a value >= all nodes in T.");
      pragma Assert (T.Last < Node_Index'Last);
      T.Last := T.Last + 1;
      T.Table (T.Last) := Node;
      New_Index := T.Last;
      pragma Assume ((if not Empty_Tree (T_In) then
             Structure_Preserved_Except (T, T_In, Last_Node_Index (T_In)) and
             Contents_Preserved_Except (T, T_In, Last_Node_Index (T_In))),
             "Adding a new node to a non-empty tree does not affect the rest " &
                       "of the Tree.");
      pragma Assert (Left (T, New_Index) = Null_Index);
      pragma Assert (Right (T, New_Index) = Null_Index);
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
      T_In : constant Tree := T with Ghost;
   begin
      T.Last := Final_Node;
      pragma Assume (Structure_Preserved_Between
                     (T, T_In, First_Node_Index, Final_Node, Null_Index) and
                       Contents_Preserved_Between
                         (T, T_In, First_Node_Index, Final_Node, Null_Index));
    end Clear_Tree_Below_Node;

end Basic_Tree;
