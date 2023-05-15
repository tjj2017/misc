pragma Ada_2012;
package body SPARK_2014.Dynamic_Tables
  with SPARK_Mode => Off
is

    ----------------
   -- Last_Index --
   ----------------

   function Last_Index (T : Table_Type) return Table_Last_Type is
   begin
      return Gnat_Table.Last (T.The_Table);
   end Last_Index;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (T : Table_Type) return Boolean is
   begin
      return Gnat_Table.Is_Empty (T.The_Table);
   end Is_Empty;

   ----------
   -- Init --
   ----------

   procedure Init (T : out Table_Type) is
   begin
      Gnat_Table.Init (T.The_Table);
   end Init;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Table_Type; New_Val : Table_Component_Type) is
   begin
      Gnat_Table.Append (T.The_Table, New_Val);
   end Append;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (T    : in out Table_Type; Index : Valid_Table_Index_Type;
      Item :        Table_Component_Type)
   is
   begin
      T.The_Table.Table (Index) := Item;
   end Set_Item;

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

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (T : in out Table_Type; New_Val : Table_Last_Type) is
   begin
      Gnat_Table.Set_Last (T.The_Table, New_Val);
   end Set_Last;

end SPARK_2014.Dynamic_Tables;
