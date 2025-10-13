with GNAT.Dynamic_Tables;
package body Basic_Tree with
  SPARK_Mode => Off
is

  package Gnat_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Actual_Node,
      Table_Index_Type     => Valid_Tree_Node,
      Table_Low_Bound      => Valid_Tree_Node'First,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Release_Threshold    => 0);

   T : Gnat_Table.Instance;

   Current_Root : Tree_Node;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Gnat_Table.Init (T);
      T.Table (Base_Node) := Null_Actual_Node;
      Current_Root := Base_Node;
   end Init;

   -------------------
   -- Is_Empty_Tree --
   -------------------

   function Is_Empty_Tree return Boolean is (Gnat_Table.Is_Empty (T));

   ---------------
   -- Base_Node --
   ---------------

   function Base_Node return Tree_Node is
     (if not Gnat_Table.Is_Empty (T) then
           Gnat_Table.First
      else
         Empty_Node);

   ----------
   -- Root --
   -----------

   function Root return Tree_Node is (Current_Root);

   --------------------------
   -- Is_A_Valid_Tree_Node --
   --------------------------

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
     (not Gnat_Table.Is_Empty (T) and then
      N in Valid_Tree_Node and then N <= Gnat_Table.Last (T));

   ------------
   -- In_Tree --
   -------------


   function In_Tree (N : Tree_Node) return Boolean is
     (Is_A_Valid_Tree_Node (N));

    -----------
   -- Level --
   -----------

   function Level (N : Valid_Tree_Node) return Level_Type is
     (T.Table (N).Level);

   ----------
   -- Left --
   ----------

   function Left (N : Valid_Tree_Node) return Tree_Node is (T.Table (N).Left);

   -----------
   -- Right --
   -----------

   function Right (N : Valid_Tree_Node) return Tree_Node is
     (T.Table (N).Right);

   ---------
   -- Key --
   ---------

   function Key (N : Valid_Tree_Node) return Key_Type is (T.Table (N).Key);

   -----------
   -- Value --
   -----------

   function Value (N : Valid_Tree_Node) return Value_Type is
     (T.Table (N).Value);

   --------------
   -- Set_Root --
   --------------

 procedure Set_Root (N : Valid_Tree_Node) is
   begin
      Current_Root := N;
   end Set_Root;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (N : Valid_Tree_Node; Node_Level : Level_Type) is
   begin
      T.Table (N).Level := Node_Level;
   end Set_Level;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (N : Valid_Tree_Node; Branch : Tree_Node) is
   begin
      T.Table (N).Left := Branch;
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (N : Valid_Tree_Node; Branch : Tree_Node) is
   begin
      T.Table (N).Right := Branch;
   end Set_Right;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (N : Valid_Tree_Node; The_Key : Key_Type) is
   begin
      T.Table (N).Key := The_Key;
   end Set_Key;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (N : Valid_Tree_Node;
                        Node_Value : Value_Type) is
   begin
      T.Table (N).Value := Node_Value;
   end Set_Value;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (N : out Valid_Tree_Node; The_Key : Key_Type) is
   begin
      Gnat_Table.Append (T, Null_Actual_Node);
      N := Gnat_Table.Last (T);
      T.Table (N).Key := The_Key;
   end Add_Node;

   -----------------------
   -- Last_Node_In_Tree --
   -----------------------

   function Last_Node return Tree_Node is
     (if Is_A_Valid_Tree_Node(Gnat_Table.Last (T))
      then
         Gnat_Table.Last (T)
      else
         Empty_Node);

   ---------------------------
   -- Clear_Tree_Below_Node --
   --------------------------

   procedure Clear_Tree_Below_Node (N : Valid_Tree_Node) is
   begin
      if N /= Empty_Node then
         Gnat_Table.Set_Last (T, N);
         T.Table (N).Left := Empty_Node;
         T.Table (N).Right := Empty_Node;
      end if;
    end Clear_Tree_Below_Node;

end Basic_Tree;
