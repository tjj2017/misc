package body SPARK_Classic.Dynamic_Tables is

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T : Table_Type) return Boolean is
   begin
      return Gnat_Table.Is_Empty (T.The_Table);
   end Is_Empty;
   pragma Inline (Is_Empty);

   ----------
   -- Init --
   ----------

   procedure Init (T : out Table_Type) is
   begin
      Gnat_Table.Init (T.The_Table);
   end Init;
   pragma Inline (Init);

   ----------
   -- Last --
   ----------

   function Last_Index (T : Table_Type) return Table_Last_Type is
   begin
      return Gnat_Table.Last (T.The_Table);
   end Last_Index;
   pragma Inline (Last_Index);

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Table_Type; New_Val : Table_Component_Type) is
   begin
      Gnat_Table.Append (T.The_Table, New_Val);
   end Append;
   pragma Inline (Append);

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (T    : in out Table_Type; Index : Valid_Table_Index_Type;
      Item :        Table_Component_Type)
   is
   begin
      Gnat_Table.Set_Item (T.The_Table, Index, Item);
   end Set_Item;
   pragma Inline (Set_Item);

   --------------
   -- Get_Item --
   --------------

   function Get_Item
     (T : Table_Type; Index : Valid_Table_Index_Type)
      return Table_Component_Type
   is
   begin
      return T.The_Table.Table (Index);
   end Get_Item;
   pragma Inline (Get_Item);

   procedure Set_Last (T : in out Table_Type; New_Val : Table_Last_Type)
   is
   begin
      Gnat_Table.Set_Last (T.The_Table, New_Val);
   end Set_Last;

end SPARK_Classic.Dynamic_Tables;

