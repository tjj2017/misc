package body SPARK_2014.Trees
is
   --  pragma Assert (Valid_Tree_Node'First =
   --                   Dynamic_Tables.Valid_Table_Index_Type'First);
   --  pragma Assert (Integer (Valid_Tree_Node'Last) = Natural'Last - 1);
   --
   ---------------
   -- Is_A_Node --
   ---------------

   function Is_A_Node (N : Tree_Node) return Boolean is (N > Empty_Node);

   function Is_Empty_Model (M : Tree_Model) return Boolean is (M'Length = 0);

   function Persist_Contents (T : Tree_Type; N : Tree_Node) return Persist_Node
   is
      (Dynamic_Tables.Get_Item (T.The_Tree, N).Conts);

   --------------
   -- To_Model --
   --------------

   function To_Model (T : Tree_Type) return Tree_Model is
      Dynamic_Array_Diff : constant Integer :=
        (if Dynamic_Tables.Is_Empty (T.The_Tree) then
              -1
         else
            Natural (Dynamic_Tables.Last_Index (T.The_Tree) -
             Dynamic_Tables.First_Index));

      Tree_Size : constant Model_Index :=
        (if Dynamic_Array_Diff < Model_Index'Last then
            Dynamic_Array_Diff + 1
         else
            Model_Index'Last);
      M  : Tree_Model (1 .. Tree_Size) :=
        (others => Model_Node'(Key => Key_Type'First, Value => Null_Value));
   begin
      if not Dynamic_Tables.Is_Empty (T.The_Tree) then
         for I in M'Range loop
            pragma Loop_Invariant (not Dynamic_Tables.Is_Empty (T.The_Tree));
            M (I) := Model_Node
              (Dynamic_Tables.Get_Item (T.The_Tree, Tree_Node (I)).Conts);
         end loop;
      end if;
      pragma Assume (Model_Equivalence (T, M));
      return M;
   end To_Model;

   function To_Tree_Node (N : Model_Index) return Tree_Node is (Tree_Node (N));

   function To_Persist_Node (M_Node : Model_Node) return Persist_Node is
     (Persist_Node (M_Node));

   --------------
   -- In_Model --
   --------------

   function In_Model (M : Tree_Model; N : Tree_Node) return Boolean is
     (Model_Index (N) in M'Range);

   ------------------
   -- Key_In_Model --
   ------------------

   --  function Key_In_Model (M : Tree_Model; K : Key_Type) return Boolean is
   --     Found : Boolean := False;
   --  begin
   --     for I in M'Range loop
   --        Found := M (I).Key = K;
   --        exit when Found;
   --     end loop;
   --     return Found;
   --  end Key_In_Model;


   -------------------
   -- Is_Empty_Tree --
   -------------------

   function Is_Empty_Tree (T : Tree_Type) return Boolean is
     (Dynamic_Tables.Is_Empty (T.The_Tree));


   ------------
   -- In_Tree --
   -------------

   function In_Tree (T : Tree_Type; N : Tree_Node) return Boolean is
      Result : constant Boolean :=
        (N > Empty_Node and then N <= Dynamic_Tables.Last_Index (T.The_Tree));
   begin
      pragma Assume (Result = (not Is_Empty_Tree (T) and
                       In_Model (To_Model (T), N)));
      return Result;
   end In_Tree;
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

   function Persists (T_Pre, T_Post : Tree_Model) return Boolean is
     (T_Pre'First = T_Post'First and T_Pre'Last = T_Post'Last and
        (for all I in T_Pre'Range => T_Pre (I) = T_Post (I) and
             In_Model (T_Pre, Tree_Node (I)) =
           In_Model (T_Post, Tree_Node (I))));

   function New_Persists (T_Pre : Tree_Model; T_Post : Tree_Type) return Boolean
   is
     (for all I in T_Pre'Range =>
         In_Model (T_Pre, Tree_Node (I)) = In_Tree (T_Post, Tree_Node (I)) and
        T_Pre (I) = Model_Node (Dynamic_Tables.Get_Item
          (T_Post.The_Tree, Tree_Node (I)).Conts) and
          In_Model (T_Pre, Tree_Node (I)) = In_Tree (T_Post, Tree_Node (I)));

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
      return Dynamic_Tables.Get_Item (T.The_Tree, N).Conts.Key;
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (T : Tree_Type; N : Tree_Node) return Value_Type
   is
   begin
      return Dynamic_Tables.Get_Item (T.The_Tree, N).Conts.Value;
   end Value;
   pragma Inline (Value);

   --------------------
   -- Key_Is_Present --
   --------------------

   function Key_Is_Present (T : Tree_Type; K : Key_Type) return Boolean is
     (for some N in Tree_Node => In_Tree (T, N) and then
      In_Model (To_Model (T), N) and then
      Key (T, N) = K);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (T : in out Tree_Type;
                        N : Tree_Node;
                        Node_Level : Natural)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Model := To_Model (T)
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Level := Node_Level;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, To_Model (T)),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (T, N),
                     "If a Node is not removed by setting its level");
      pragma Assume (Level (T, N) = Node_Level,
                     "The Level component of the node is set to Node_Level");
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
      T_Entry : constant Tree_Model := To_Model (T)
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Left := Branch;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, To_Model (T)),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (T, N),
                     "If a Node is not removed by setting its left branch");
      pragma Assume (Left (T, N) = Branch,
                     "The Left component of the node is set to Branch");
      pragma Assume (In_Model (T_Entry, Branch) = In_Tree (T, Branch),
                     "If the node is in the model it is also in the Tree");
      pragma Assume (Model_Equivalence (T, To_Model (T)));
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
      T_Entry : constant Tree_Model := To_Model (T)
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Right := Branch;
      pragma Assert (not Dynamic_Tables.Is_Empty (T.The_Tree));
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, To_Model (T)),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (T, N),
                     "If a Node is not removed by setting its right branch");
      pragma Assume (Right (T, N) = Branch,
                     "The Right component of the node is set to Branch");
      pragma Assume ((if In_Model (T_Entry, Branch) then In_Tree (T, Branch)),
                    "If the node is in the model it is also in the Tree");
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (T : in out Tree_Type; N : Tree_Node;
                      The_Key : Key_Type)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Model := To_Model (T)
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Conts.Key := The_Key;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, To_Model (T)),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (T, N),
                     "If a Node is not removed by setting its key");
      pragma Assume (Key_Is_Present (T, The_Key),
                     "The key has just been set.");
      pragma Assume (Key (T, N) = The_Key,
                     "The  Key component of the node is set to The_Key");
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
      T_Entry : constant Tree_Model := To_Model (T)
        with Ghost;
   begin
      Node_Contents := Dynamic_Tables.Get_Item (T.The_Tree, N);
      Node_Contents.Conts.Value := Node_Value;
      Dynamic_Tables.Set_Item (T.The_Tree, N, Node_Contents);
     --# accept W, 444, "Setting a component of a tree does not remove Nodes";
     --# assume Persists (T~, T);
      pragma Assume (Persists (T_Entry, To_Model (T)),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (T, N),
                     "If a Node is not removed by setting its value");
      pragma Assume (Value (T, N) = Node_Value,
                     "The Value component of the node is set to Node_Value");
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
      T_Entry : constant Tree_Model := To_Model (T)
        with Ghost;
   begin
      Node := Actual_Node'
        (Conts => Persist_Node'(Key => The_Key, Value => Null_Value),
         Level => 1,
         Left  => Empty_Node,
         Right => Empty_Node);
      Dynamic_Tables.Append (T.The_Tree, Node);
      N := Dynamic_Tables.Last_Index (T.The_Tree);
      --# accept W, 444, "Setting a component of a tree does not remove Nodes";
      --# assume Persists (T~, T);
      pragma Assert (not Is_Empty_Tree (T));
      pragma Assume (Persists (T_Entry, To_Model (T)),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (T, N),
                    "The node N has been added to the tree");
      pragma Assume (Key_Is_Present (T, The_Key),
                     "The_node N, has a Key component set to The_Key");
      pragma Assume (Key (T, N) = The_Key,
                     "A node with Key = The Key has been created by Append");
      pragma Assume (Left (T, N) = Empty_Node,
                     "Left and Right compoments of a new node are set to Empty");
      pragma Assume (Right (T, N) = Empty_Node,
                     "Left and Right compoments of a new node are set to Empty");
      pragma Assume (Value (T, N) = Null_Value,
                     "Value compoment of a new node is set to Null_Value");
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
      pragma Assume (not In_Tree (T, N));
   end Clear;

end SPARK_2014.Trees;
