--------------------------  SPARK_2014.Tree_Abs  -----------------------------                                       --
--  This package body has to be excluded from SPARK analysis as the body    --
-- has state but its specification does not                                 --
------------------------------------------------------------------------------
with GNAT.Dynamic_Tables;
package body SPARK_2014.Multi_Atree.Tree_Abs with
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

   --------------------------
   -- Is_A_Valid_Tree_Node --
   --------------------------

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
     (N >= Tree_Node (Valid_Tree_Node'First) and then
      N <= Tree_Node (Gnat_Table.Last (T)));

   ------------
   -- In_Tree --
   -------------


   function In_Tree (N : Tree_Node) return Boolean is
      Result : Boolean;
   begin
      Result := Is_A_Valid_Tree_Node (N);
      return Result;
   end In_Tree;
   pragma Inline (In_Tree);

    -----------
   -- Level --
   -----------

   function Level (N : Valid_Tree_Node) return Node_Count is
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

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (N : in out Valid_Tree_Node; Node_Level : Node_Count) is
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

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (N : in out Valid_Tree_Node;
                        Node_Value : Value_Type) is
   begin
      T.Table (N).Value := Node_Value;
   end Set_Value;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (N : out Valid_Tree_Node; The_Key : Key_Type) is
      Node : Actual_Node;
   begin
      Node := Actual_Node'
        (Key   => The_Key,
         Value => Null_Value,
         Level => 1,
         Left  => Empty_Node,
         Right => Empty_Node);
      Gnat_Table.Append (T, Node);
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
         Gnat_Table.Set_Last (T, Valid_Tree_Node (New_Last));
      end if;
      N := Empty_Node;
    end Clear_Tree_Below_Node;

   -------------
   --  Free   --
   -------------

procedure Free is
begin
   Gnat_Table.Free (T);
end Free;

end SPARK_2014.Multi_Atree.Tree_Abs;
