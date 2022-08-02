with Gnat.Table;
package body SPARK_Classic.Symbols.List_Store
with Refined_State => (Store => (List_Table.Table, List_Table.Locked))
is
   --  Initial size of node lists table;
   List_Store_Initial_Size : constant := 1_200;
   --  Growth rate.
   List_Store_Increment    : constant := 100;

 package List_Table is new GNAT.Table
     (Table_Component_Type => Store_Node,
      Table_Index_Type     => Table_Index,
      Table_Low_Bound      => Table_Index'First,
      Table_Initial        => List_Store_Initial_Size,
      Table_Increment      => List_Store_Initial_Size);


   ------------
   -- Append --
   ------------

   procedure Append (S_Node : Store_Node) renames List_Table.Append;
   --  --# global in out List_Table.Table, List_Table.Locked;

   ----------
   -- Last --
   ----------

   function Last return Table_Index renames List_Table.Last;
   --  --# global in List_Table.Table;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item (N_List : Table_Index; Item : Store_Node) renames
     List_Table.Set_item;
   --  --# global in out List_Table.Table, List_Table.Locked;

    --------------
   -- Get_Item --
   --------------

  function Get_Item (Index : Table_Index) return Store_Node is
   --  --# global in List_Table.Table;
   begin
        return List_Table.Table (Index);
   end Get_Item;
   pragma Inline (Get_Item);


end SPARK_Classic.Symbols.List_Store;
