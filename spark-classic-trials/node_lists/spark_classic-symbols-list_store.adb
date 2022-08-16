with Gnat.Dynamic_Tables;
package body SPARK_Classic.Symbols.List_Store
with Refined_State => (Store => (Store_Table,
                                 List_Table.Empty_Table_Array,
                                 List_Table.Empty_Table_Ptr))
  --  --# own Store is Store_Table,
  --  --#     List_Table.Empty_Table_Array,
  --  --#     List_Table.Empty_Table_Ptr;
is
   --  Initial size of node lists table;
   List_Store_Initial_Size : constant := 1_200;
   --  Growth rate.
   List_Store_Increment    : constant := 100;

   package List_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Store_Node,
      Table_Index_Type     => Table_Index,
      Table_Low_Bound      => Table_Index'First,
      Table_Initial        => List_Store_Initial_Size,
      Table_Increment      => List_Store_Initial_Size);

   Store_Table : List_Table.Instance;

   ------------
   -- Append --
   ------------

   procedure Append (S_Node : Store_Node)
   --  --# global in out Store_Table;
   with Refined_Global => (In_Out => Store_Table)
   is
   begin
      List_Table.Append (Store_Table, S_Node);
   end Append;
   pragma Inline (Append);

   --------------
   -- Item --
   --------------

  function Item (Index : Table_Index) return Store_Node
   --  --# global in Store_Table;
   with Refined_Global => (Input => Store_Table)
   is
   begin
        return  Store_Table.Table (Index);
   end Item;
   pragma Inline (Item);

   ----------
   -- Last --
   ----------

   function Last return Table_Index
   --  --# global in Store_Table;
   with Refined_Global => (Input => Store_Table)
   is
   begin
      return List_Table.Last (Store_Table);
   end Last;
   pragma Inline (Last);

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item (N_List : Table_Index; Item : Store_Node)
   --  --# global in out Store_Table;
   with Refined_Global => (In_Out => Store_Table)
   is
   begin
      List_Table.Set_Item (Store_Table, N_List, Item);
   end Set_Item;
   pragma Inline (Set_Item);

end SPARK_Classic.Symbols.List_Store;
