--------------------------  SPARK_2014.Tree_Abs  -----------------------------
--  A thin interface which provides a stateless abstraction to the          --
--  SPARK_2014-Dynamic_Trees package.                                       --
--  This package body has to be excluded from SPARK analysis as the body    --
-- has state but its specification does not
------------------------------------------------------------------------------
with SPARK_2014.Multi_Atree.Tree_Abs.Tree;
package body SPARK_2014.Multi_Atree.Tree_Abs with
  SPARK_Mode => Off
is

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (N : Tree_Node) return Boolean is (N = Empty_Node);

   --------------------
   --  Key_Of_Node  --
   --------------------
   function Key_Of_Node (N : Tree_Node; T : A_Tree) return Key_Type is
     (Tree.Key (N));

   --  ---------------
   --  -- Is_A_Node --
   --  ---------------
   --
   --  function Is_A_Node (N : Tree_Node) return Boolean is (N > Empty_Node);
   --
   --  --------------------------
   --  -- Is_A_Valid_Tree_Node --
   --  --------------------------
   --
   --  function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
   --    (Tree.Is_A_Valid_Tree_Node (N));
   --
   --
   --  ------------
   --  -- In_Tree --
   --  -------------
   --
   --
   --  function In_Tree (N : Tree_Node) return Boolean is
   --     Result : Boolean;
   --  begin
   --     Result := Tree.Is_A_Valid_Tree_Node (N);
   --     pragma Assume (if Result then not Is_Empty (N));
   --     return Result;
   --  end In_Tree;
   --  pragma Inline (In_Tree);
   --
   --  --------------------
   --  -- Key_Is_Present --
   --  --------------------
   --
   --  function Key_Is_Present (K : Key_Type; T : Tree_Node) return Boolean is
   --    (for some N in Tree_Node => In_Tree (N) and then Key (N) = K);
   --
   --  ---------------------
   --  -- Node_Is_Present --
   --  ---------------------
   --
   --  function Node_Is_Present (N : Tree_Node) return Boolean is
   --    (Tree.In_Tree (N));
   --  pragma Inline (Node_Is_Present);

   --------------
   -- Persists --
   --------------

   --  function Persists (T_Pre, T_Post : Tree_Node) return Boolean
   --  is
   --  begin
   --    return Tree.Persists (Tree.TS (T_Pre), Tree.TS (T_Post));
   --  end Persists;
   --
    -----------
   -- Level --
   -----------

   function Level (T : A_Tree) return Natural is
   begin
      if Empty_Tree (T)  then
         return 0;
      else
         return (Tree.Level (T.Target_Node));
      end if;
   end Level;
   pragma Inline (Level);

   ----------
   -- Left --
   ----------

   function Left (T : A_Tree) return Tree_Node is
      L : Tree_Node;
   begin
      L := Tree.Left (T.Target_Node);
      pragma Assume ((if L > Empty_Node then In_A_Tree (L, T)),
                    "The left branch is either empty or In_A_Tree");
      return L;
   end Left;
   pragma Inline (Left);

   -----------
   -- Right --
   -----------

   function Right (T : A_Tree) return Tree_Node is
      R : Tree_Node;
   begin
      R := Tree.Right (T.Target_Node);
      pragma Assume ((if R > Empty_Node then In_A_Tree (R, T)),
                    "The right branch is either empty or In_A_Tree");
      return R;
   end Right;
   pragma Inline (Right);

   ---------
   -- Key --
   ---------

   function Key (T : A_Tree) return Key_Type is
   begin
     return Tree.Key (T.Target_Node);
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (T : A_Tree) return Value_Type is
   begin
     return Tree.Value (T.Target_Node);
   end Value;
   pragma Inline (Value);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (T : in out A_Tree; Node_Level : Natural) is
   begin
      Tree.Set_Level (T.Target_Node, Node_Level);

      pragma Assume (Target_Node_In_Tree (T),
                     "Target_Node_In_Tree (T)'Old -> Target_Node_In_Tree (T)");
      pragma Assume (Level (T) = Node_Level,
                     "The Node_Level component has been set to Node_Level");
   end Set_Level;
   pragma Inline (Set_Level);

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (T : in out A_Tree; Branch : Tree_Node) is
      Entry_T : constant A_Tree := T with Ghost;
   begin
      Tree.Set_Left (T.Target_Node, Branch);

      pragma Assume (Target_Node_In_Tree (T),
                     "Target_Node_In_Tree (T)'Old -> Target_Node_In_Tree (T)");
      pragma Assume ((if In_A_Tree (Branch, Entry_T) then
                        In_A_Tree (Branch, T)),
                     "The Tree still contains the node, Branchh");
      pragma Assume (Left (T) = Branch,
                     "The Left component has been set to Branch");
   end Set_Left;
   pragma Inline (Set_Left);

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (T : in out A_Tree; Branch : Tree_Node) is
      Entry_T : constant A_Tree := T with Ghost;
begin
      Tree.Set_Right (T.Target_Node, Branch);

      pragma Assume (Target_Node_In_Tree (T),
                     "Target_Node_In_Tree (N)'Old -> Target_Node_In_Tree (T)");
      pragma Assume ((if In_A_Tree (Branch, Entry_T) then
                        In_A_Tree (Branch, T)),
                     "The Tree still contains the node, Branch.");
      pragma Assume (Right (T) = Branch,
                     "The Right component has been set to Branch");
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (T : in out A_Tree; The_Key : Key_Type) is
   begin
      Tree.Set_Key (T.Target_Node, The_Key);

      pragma Assume (Target_Node_In_Tree (T),
                     "The Target_Node is unchanged");
      pragma Assume (Key (T) = The_Key,
                     "The key has just been set.");
   end Set_Key;
   pragma Inline (Set_Key);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (T : in out A_Tree;
                        Node_Value : Value_Type) is
   begin
      Tree.Set_Value (T.Target_Node, Node_Value);

      pragma Assume (Target_Node_In_Tree (T),
                     "The Target_Node is unchanged");
      pragma Assume (Value (T) = Node_Value,
                     "The Value has been set to Node_Value");
   end Set_Value;
   pragma Inline (Set_Value);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (T : in out A_Tree; The_Key : Key_Type) is
   begin
      Tree.Add_Node (T.Target_Node, The_Key);

      pragma Assume (Target_Node_In_Tree (T),
                     "The Target_Node is the new node added to the tree.");
        pragma Assume (Key (T) = The_Key,
                    "A node with a Key = The Key has been created by Append");
   end Add_Node;
   pragma Inline (Add_Node);

   -----------
   -- Clear --
   -----------

   procedure Clear (T : in out A_Tree) is
   begin
      Tree.Clear (T.Root);
      T.Root := Empty_Node;
      T.Target_Node := Empty_Node;
   end Clear;

end SPARK_2014.Multi_Atree.Tree_Abs;
