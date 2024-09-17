--------------------  SPARK_2014.Tree_Abstraction  ---------------------------
--  This package body has to be excluded from SPARK analysis as the body    --
--  has state but its specification does not declare an abstract state      --
------------------------------------------------------------------------------
with GNAT.Dynamic_Tables;
package body SPARK_2014.Tree_Abstraction with
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

   procedure Init is
   begin
      Gnat_Table.Init (T);
   end Init;

   --------------------------
   -- Is_A_Valid_Tree_Node --
   --------------------------

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean is
      (N in Valid_Tree_Node and N <= Gnat_Table.Last (T));

   ------------
   -- In_Tree --
   -------------


   function In_Tree (N : Tree_Node) return Boolean is
      Result : Boolean;
   begin
      Result := Is_A_Valid_Tree_Node (N);
      return Result;
   end In_Tree;

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

end SPARK_2014.Tree_Abstraction;
