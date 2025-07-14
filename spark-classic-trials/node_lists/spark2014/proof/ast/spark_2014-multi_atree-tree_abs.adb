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

   --  --------------
   --  -- Is_Empty --
   --  --------------
   --  function Is_Empty (N : Tree_Node) return Boolean is (N = Empty_Node);

   --------------------
   --  Key_Of_Node  --
   --------------------
   --  function Key_Of_Node (N : Tree_Node; N : Tree_Node) return Key_Type is
   --    (Tree.Key (N));

   --  ---------------
   --  -- Is_A_Node --
   --  ---------------
   --
   --  function Is_A_Node (N : Tree_Node) return Boolean is (N > Empty_Node);
   --
   --------------------------
   -- Is_A_Valid_Tree_Node --
   --------------------------

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
     (Tree.Is_A_Valid_Tree_Node (N));


   ------------
   -- In_Tree --
   -------------


   function In_Tree (N : Tree_Node) return Boolean is
      Result : Boolean;
   begin
      Result := Tree.Is_A_Valid_Tree_Node (N);
      pragma Assume (if Result then not Is_Empty (N));
      return Result;
   end In_Tree;
   pragma Inline (In_Tree);

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

   function Level (N : Tree_Node) return Node_Count is
   begin
      if Is_Empty (N)  then
         return 0;
      else
         return (Tree.Level (N));
      end if;
   end Level;
   pragma Inline (Level);

   ----------
   -- Left --
   ----------

   function Left (N : Tree_Node) return Tree_Node is
      L : Tree_Node;
   begin
      L := Tree.Left (N);
      pragma Assume ((if L > Empty_Node then In_Tree (L)),
                    "The left branch is either empty or In_Tree");
      return L;
   end Left;
   pragma Inline (Left);

   -----------
   -- Right --
   -----------

   function Right (N : Tree_Node) return Tree_Node is
      R : Tree_Node;
   begin
      R := Tree.Right (N);
      pragma Assume ((if R > Empty_Node then In_Tree (R)),
                    "The right branch is either empty or In_A_Tree");
      return R;
   end Right;
   pragma Inline (Right);

   ---------
   -- Key --
   ---------

   function Key (N : Tree_Node) return Key_Type is
   begin
     return Tree.Key (N);
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (N : Tree_Node) return Value_Type is
   begin
     return Tree.Value (N);
   end Value;
   pragma Inline (Value);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (N : in out Tree_Node; Node_Level : Node_Count) is
   begin
      Tree.Set_Level (N, Node_Level);

      pragma Assume (In_Tree (N),
                     "Node_In_Tree (N)'Old -> Node_In_Tree (N)");
      pragma Assume (Level (N) = Node_Level,
                     "The Node_Level component has been set to Node_Level");
   end Set_Level;
   pragma Inline (Set_Level);

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (N : in out Tree_Node; Branch : Tree_Node) is
   begin
      Tree.Set_Left (N, Branch);

      pragma Assume (In_Tree (N),
                     "Node_In_Tree (N)'Old -> Node_In_Tree (N)");
      pragma Assume (Left (N) = Branch,
                     "The Left component has been set to Branch");
   end Set_Left;
   pragma Inline (Set_Left);

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (N : in out Tree_Node; Branch : Tree_Node) is
   begin
      Tree.Set_Right (N, Branch);

      pragma Assume (In_Tree (N),
                     "In_Tree (N)'Old -> In_Tree (N)");
      pragma Assume (Right (N) = Branch,
                     "The Right component has been set to Branch");
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (N : in out Tree_Node; The_Key : Key_Type) is
   begin
      Tree.Set_Key (N, The_Key);

      pragma Assume (In_Tree (N),
                     "In_Tree (N)'Old -> In_Tree (N)");
      pragma Assume (Key (N) = The_Key,
                     "The key has just been set.");
   end Set_Key;
   pragma Inline (Set_Key);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (N : in out Tree_Node;
                        Node_Value : Value_Type) is
   begin
      Tree.Set_Value (N, Node_Value);

      pragma Assume (In_Tree (N),
                     "In_Tree (N)'Old -> In_Tree (N)");
      pragma Assume (Value (N) = Node_Value,
                     "The Value has been set to Node_Value");
   end Set_Value;
   pragma Inline (Set_Value);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (N : out Tree_Node; The_Key : Key_Type) is
   begin
      Tree.Add_Node (N, The_Key);

      pragma Assume (In_Tree (N),
                     "N is the new node added to the tree.");
       pragma Assume (Key (N) = The_Key,
                    "A node with a Key = The Key has been created by Append");
   end Add_Node;
   pragma Inline (Add_Node);

   -----------
   -- Clear --
   -----------

   procedure Clear (N : in out Tree_Node) is
   begin
      Tree.Clear (N);
   end Clear;

end SPARK_2014.Multi_Atree.Tree_Abs;
