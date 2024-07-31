----------------------------  SPARK_2014.Tree  -------------------------------
--  A specialisation of the SPARK_2014-Dynamic_Trees package.               --
--  It in turn uses a specialised SPARK_2014.Dynamic_Tables package,        --                          --
--  SPARK_2014-Tree-Dynamic_Table, which is a private child of this package --
------------------------------------------------------------------------------
with SPARK_2014.Tree_Abs.Tree.Dynamic_Table;
package body SPARK_2014.Tree_Abs.Tree with
  SPARK_Mode => Off,
  Refined_State => (Tree_Store => Dynamic_Table.The_Table)
is

   function TS return Tree_Type with
     Refined_Global => Dynamic_Table.The_Table,
     SPARK_Mode => Off
     -- SPARK-Mode off because partial but sufficient initialisation of TS.
   is
      Result : Tree_Type;
   begin
      Result.Is_Empty := Dynamic_Table.Is_Empty;
      if not Result.Is_Empty then
         for I in Valid_Tree_Node'First ..
           Dynamic_Table.Last_Index
         loop
            pragma Loop_Invariant (I <= Result.Tree'Last);
            Result.Tree (I) :=
              Dynamic_Table.Get_Item (I);
         end loop;
      end if;
      return Result;
   end TS;

   --  Ghost functions which use the Ghost Type representing the type
   --  of the Abstract_State Tree_Store.

   function Is_Empty_Tree (T : Tree_Type) return Boolean is (T.Is_Empty);

   function In_Tree_Ghost (T : Tree_Type; N : Tree_Node) return Boolean is
     (not Is_Empty_Tree (T) and N in Valid_Tree_Node) with Ghost;

   function Key_Ghost (T : Tree_Type; N : Tree_Node) return Key_Type is
     (T.Tree (N).Key) with
   Pre => not T.Is_Empty and
          N in Valid_Tree_Node,
   Ghost;

   function Key_Is_Present_Ghost (T : Tree_Type; K : Key_Type) return Boolean
   is (if not Is_Empty_Tree (T)  then
         (for some N in Tree_Node => In_Tree_Ghost (T, N) and then
          Key_Ghost (T, N) = K)
       else
          False) with
 --  Pre => not Is_Empty_Tree (T),
   Ghost;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty return Boolean is (Dynamic_Table.Is_Empty) with
     Refined_Global => Dynamic_Table.The_Table;

   ---------------
   -- Is_A_Node --
   ---------------

   function Is_A_Node (N : Tree_Node) return Boolean is (N > Empty_Node);

   --------------------------
   -- Is_A_Valid_Tree_Node --
   --------------------------

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
     (N in Valid_Tree_Node and then N <= Dynamic_Table.Last_Index)
   with
     Refined_Global => Dynamic_Table.The_Table;


   ------------
   -- In_Tree --
   -------------


   function In_Tree (N : Tree_Node) return Boolean with
     Refined_Global => Dynamic_Table.The_Table
   is
      Result : Boolean;
   begin
      Result := Is_A_Valid_Tree_Node (N);
      pragma Assume (if Result then not Is_Empty);
      return Result;
   end In_Tree;
   pragma Inline (In_Tree);

   --------------------
   -- Key_Is_Present --
   --------------------

   function Key_Is_Present (K : Key_Type) return Boolean with
       Refined_Global => Dynamic_Table.The_Table
   is
   begin
      return (for some N in Tree_Node => In_Tree (N) and then Key (N) = K);
   end Key_Is_Present;

   ---------------------
   -- Node_Is_Present --
   ---------------------

   function Node_Is_Present (N : Tree_Node) return Boolean is (In_Tree (N))
     with Refined_Global => Dynamic_Table.The_Table;
   pragma Inline (Node_Is_Present);

   --------------
   -- Persists --
   --------------

   function Persists (T_Pre, T_Post : Tree_Type) return Boolean
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
     Refined_Global => Dynamic_Table.The_Table
   is
   begin
      if Dynamic_Table.Is_Empty then
         return 0;
      else
         return (Dynamic_Table.Get_Item (N).Level);
      end if;
   end Level;
   pragma Inline (Level);

   ----------
   -- Left --
   ----------

   function Left (N : Tree_Node) return Tree_Node with
     Refined_Global => Dynamic_Table.The_Table
   is
      L : Tree_Node;
   begin
      L := Dynamic_Table.Get_Item (N).Left;
      pragma Assume ((if L > Empty_Node then In_Tree (L)),
                    "The left branch is either empty or In_Tree");
      return L;
   end Left;
   pragma Inline (Left);

   -----------
   -- Right --
   -----------

   function Right (N : Tree_Node) return Tree_Node with
     Refined_Global => Dynamic_Table.The_Table
   is
      R : Tree_Node;
   begin
      R := Dynamic_Table.Get_Item (N).Right;
      pragma Assume ((if R > Empty_Node then In_Tree (R)),
                    "The right branch is either empty or In_Tree");
      return R;
   end Right;
   pragma Inline (Right);

   ---------
   -- Key --
   ---------

   function Key (N : Tree_Node) return Key_Type with
     Refined_Global => Dynamic_Table.The_Table
   is
   begin
     return (Dynamic_Table.Get_Item (N).Key);
   end Key;
   pragma Inline (Key);

   -----------
   -- Value --
   -----------

   function Value (N : Tree_Node) return Value_Type with
     Refined_Global => Dynamic_Table.The_Table
   is
   begin
     return (Dynamic_Table.Get_Item (N).Value);
   end Value;
   pragma Inline (Value);

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (N : Tree_Node;
                        Node_Level : Natural) with
     Refined_Global => (In_Out => Dynamic_Table.The_Table)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Table.Get_Item (N);
      Node_Contents.Level := Node_Level;
      Dynamic_Table.Set_Item (N, Node_Contents);

      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (N),
                     "In_Tree (N)'Old -> In_Tree (N)");
      pragma Assume (Level (N) = Node_Level,
                     "The Node_Level component has been set to Node_Level");
      pragma Assume (not Is_Empty_Tree (TS),
                     "The tree is not empty on entry");

   end Set_Level;
   pragma Inline (Set_Level);

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (N : Tree_Node;
                       Branch : Tree_Node) with
     Refined_Global => (In_Out => Dynamic_Table.The_Table)
   is
      Node_Contents : Actual_Node;
      T_Entry   : constant Tree_Type := TS with Ghost;
      B_In_Tree : constant Boolean := In_Tree (Branch) with Ghost;
   begin
      Node_Contents := Dynamic_Table.Get_Item (N);
      Node_Contents.Left := Branch;
      Dynamic_Table.Set_Item (N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (N),
                     "In_Tree (N)'Old -> In_Tree (N)");
      pragma Assume ((if B_In_Tree then In_Tree (Branch)),
                     "The Left component is set tho Branch");
      pragma Assume (Left (N) = Branch,
                     "The Left component has been set to Branch");
      pragma Assume (not Is_Empty_Tree (TS),
                     "The tree is not empty on entry");
   end Set_Left;
   pragma Inline (Set_Left);

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (N : Tree_Node;
                        Branch : Tree_Node) with
     Refined_Global => (In_Out => Dynamic_Table.The_Table)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
      B_In_Tree : constant Boolean := In_Tree (Branch) with Ghost;
   begin
      Node_Contents := Dynamic_Table.Get_Item (N);
      Node_Contents.Right := Branch;
      Dynamic_Table.Set_Item (N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (In_Tree (N),
                     "In_Tree (N)'Old -> In_Tree (N)");
      pragma Assume ((if B_In_Tree then In_Tree (Branch)),
                     "The Right component is set tho Branch");
      pragma Assume (Right (N) = Branch,
                     "The Right component has been set to Branch");
      pragma Assume (not Is_Empty_Tree (TS),
                     "The tree is not empty on entry");
   end Set_Right;
   pragma Inline (Set_Right);

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (N : Tree_Node; The_Key : Key_Type) with
     Refined_Global => (In_Out => Dynamic_Table.The_Table)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Table.Get_Item (N);
      Node_Contents.Key := The_Key;
      Dynamic_Table.Set_Item (N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (Key_Is_Present (The_Key),
                     "The key has just been set.");
      pragma Assume (not Is_Empty_Tree (TS),
                     "The tree is not empty on entry");
   end Set_Key;
   pragma Inline (Set_Key);

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (N : Tree_Node;
                        Node_Value : Value_Type) with
     Refined_Global => (In_Out => Dynamic_Table.The_Table)
   is
      Node_Contents : Actual_Node;
      T_Entry : constant Tree_Type := TS with Ghost;
   begin
      Node_Contents := Dynamic_Table.Get_Item (N);
      Node_Contents.Value := Node_Value;
      Dynamic_Table.Set_Item (N, Node_Contents);
      pragma Assume (Persists (T_Entry, TS),
                     "Setting a component of a tree does not remove Nodes");
      pragma Assume (not Is_Empty_Tree (TS),
                     "The tree is not empty on entry");
   end Set_Value;
   pragma Inline (Set_Value);

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (N : out Tree_Node;
                       The_Key : Key_Type) with
     Refined_Global => (In_Out => Dynamic_Table.The_Table)
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
      Dynamic_Table.Append (Node);
      pragma Assume (Persists (T_Entry, TS),
                     "Adding a Node to a tree does not remove Nodes");
      N := Dynamic_Table.Last_Index;
      pragma Assume (In_Tree (N) and then Key (N) = The_Key,
                    "A node with a Key = The Key has been created by Append");
      pragma Assume (not Is_Empty_Tree (TS),
                     "The tree has a node added");
   end Add_Node;
   pragma Inline (Add_Node);

   -----------
   -- Clear --
   -----------

   procedure Clear (N : Tree_Node) with
   Refined_Global => (In_Out => Dynamic_Table.The_Table)
   is
      New_Last : Tree_Node;
   begin
      if N /= Empty_Node then
         New_Last := N - 1;
      else
         New_Last := N;
      end if;

      Dynamic_Table.Set_Last (New_Val => New_Last);
   end Clear;

end SPARK_2014.Tree_Abs.Tree;
