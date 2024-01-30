pragma SPARK_Mode (On);
with GNAT.Dynamic_Tables;
generic
   type Table_Component_Type is private;
   type Table_Index_Type     is range <>;

   Table_Low_Bound   : Table_Index_Type := Table_Index_Type'First;
   Table_Initial     : Positive := 8;
   Table_Increment   : Natural := 100;
   Release_Threshold : Natural := 0; -- size in bytes

package SPARK_2014.Dynamic_Tables
is
   type Table_Type is private;  -- with No_Copy;

   subtype Valid_Table_Index_Type is Table_Index_Type'Base
     range Table_Low_Bound .. Table_Index_Type'Base'Last;
   subtype Table_Last_Type is Table_Index_Type'Base
     range Table_Low_Bound - 1 .. Table_Index_Type'Base'Last;

   function Last_Index (T : Table_Type) return Table_Last_Type with
   Post => Last_Index'Result <= Valid_Table_Index_Type'Last;
   --  Returns the current value of the last used entry in the table, which can
   --  then be used as a subscript for Table.

   function First_Index return Table_Last_Type with
   Post => First_Index'Result <= Valid_Table_Index_Type'Last;
   --  Returns the current value of the first used entry in the table, which can
   --  then be used as a subscript for Table.

   function Is_Empty (T : Table_Type) return Boolean with
   Post => (if not Is_Empty'Result then Last_Index (T) > First_Index);

   procedure Init (T : out Table_Type)
     with Post => Is_Empty (T);
   --  Reinitializes the table to empty. There is no need to call this before
   --  using a table; tables default to empty.

   procedure Append (T : in out Table_Type; New_Val : Table_Component_Type)
     with Post => not Is_Empty (T) and
                  (Last_Index (T) = Last_Index (T)'Old + 1) and
                  (Last_Index (T) < Table_Last_Type'Last);
   --  Appends New_Val onto the end of the table
   --  Equivalent to:
   --    Increment_Last_Index (T);
   --    T.Table (T.Last_Index) := New_Val;

   procedure Set_Item
     (T     : in out Table_Type;
      Index : Valid_Table_Index_Type;
      Item  : Table_Component_Type) with
     Pre  => not Is_Empty (T),
     Post => not Is_Empty (T);
   --  Put Item in the table at position Index. If Index points to an existing
   --  item (i.e. it is in the range First .. Last_Index (T)), the item is
   --  replaced. Otherwise (i.e. Index > Last_Index (T)), the table is
   --  expanded, and Last_Index is set to Index.

   function Get_Item (T : Table_Type; Index : Valid_Table_Index_Type)
                      return Table_Component_Type with
     Pre => not Is_Empty (T);

   procedure Set_Last (T : in out Table_Type; New_Val : Table_Last_Type)
     with Post => Last_Index (T) = New_Val;
   --  This procedure sets Last_Index to the indicated value. If necessary the
   --  table is reallocated to accommodate the new value (i.e. on return the
   --  allocated table has an upper bound of at least Last). If Set_Last
   --  reduces the size of the table, then logically entries are removed from
   --  the table. If Set_Last increases the size of the table, then new entries
   --  are logically added to the table.

private
   pragma SPARK_Mode (Off);

   package Gnat_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Table_Component_Type,
      Table_Index_Type     => Table_Index_Type,
      Table_Low_Bound      => Table_Low_Bound,
      Table_Initial        => Table_Initial,
      Table_Increment      => Table_Increment);

   type Table_Type is record
      The_Table : Gnat_Table.Instance;
   end record;

end SPARK_2014.Dynamic_Tables;

