--------------------  SPARK_2014.Tree_Abstraction  ---------------------------
--  This package body has to be excluded from SPARK analysis as the body    --
--  has state but its specification does not declare an own variable.       --
------------------------------------------------------------------------------
with GNAT.Dynamic_Tables;
package body SPARK_2005.Multi_ATree.Tree_Abstraction is
--# hide SPARK_2005.Multi_ATree.Tree_Abstraction;

  package Gnat_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Actual_Node,
      Table_Index_Type     => Valid_Tree_Node,
      Table_Low_Bound      => Valid_Tree_Node'First,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Release_Threshold    => 0);

      T : Gnat_Table.Instance;

   procedure Init is
   begin
      Gnat_Table.Init (T);
   end Init;

   --------------------------
   -- Is_A_Valid_Tree_Node --
   --------------------------

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
   begin
      return (N in Valid_Tree_Node and N <= Gnat_Table.Last (T));
   end Is_A_Valid_Tree_Node;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (N :  Multi_ATree.Tree_Node) return Boolean is
   begin
      return N = Multi_Atree.Empty_Node;
   end Is_Empty;

    ------------
   -- In_Tree --
   -------------

   function In_Tree (N : Tree_Node) return Boolean is
   begin
      return Is_A_Valid_Tree_Node (N);
   end In_Tree;

    -----------
   -- Level --
   -----------

   function Level (N : Valid_Tree_Node) return Level_Type is
   begin
      return (T.Table (N).Level);
   end Level;

   ----------
   -- Left --
   ----------

   function Left (N : Valid_Tree_Node) return Tree_Node is
   begin
      return (T.Table (N).Left);
   end Left;

   -----------
   -- Right --
   -----------

   function Right (N : Valid_Tree_Node) return Tree_Node is
   begin
      return (T.Table (N).Right);
   end Right;

   ---------
   -- Key --
   ---------

   function Key (N : Valid_Tree_Node) return Key_Type is
   begin
      return (T.Table (N).Key);
   end Key;

    ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (N : in out Valid_Tree_Node; Node_Level : Level_Type) is
   begin
      T.Table (N).Level := Node_Level;
   end Set_Level;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left (N : in out Valid_Tree_Node; Branch : Tree_Node) is
   begin
      T.Table (N).Left := Branch;
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right (N : in out Valid_Tree_Node; Branch : Tree_Node) is
   begin
      T.Table (N).Right := Branch;
   end Set_Right;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (N : in out Valid_Tree_Node; The_Key : Key_Type) is
   begin
      T.Table (N).Key := The_Key;
   end Set_Key;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (N : out Valid_Tree_Node; The_Key : Key_Type) is
   begin
      Gnat_Table.Append (T, Null_Actual_Node);
      N := Gnat_Table.Last (T);
   end Add_Node;

   ---------------------------
   -- Clear_Tree_Below_Node --
   --------------------------

   procedure Clear_Tree_Below_Node (N : in out  Tree_Node) is
      New_Last : Tree_Node;
   begin
      New_Last := N - 1;
      if New_Last /= Empty_Node then
         Gnat_Table.Set_Last (T, New_Last);
      end if;
      N := Empty_Node;
    end Clear_Tree_Below_Node;

end SPARK_2005.Multi_ATree.Tree_Abstraction;
