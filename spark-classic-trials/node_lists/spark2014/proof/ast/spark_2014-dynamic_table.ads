--------------------  SPARK_2014.Trees.Dynamic_Table ------------------------
--  This is a specialisation of the package SPARK_2014-Dynamic_Tables      --
--  This package specification has its body excluded from SPARK 2014       --
--  as, with the 2021 version of SPARK being used, SPARK_Mode => Off does  --
--  not entirely prevent analysis of the body and SPARK 2014 is unable to  --
--  the resolve the state refinement properly.                             --
-----------------------------------------------------------------------------
pragma SPARK_Mode (On);
package SPARK_2014.Dynamic_Table with
   Abstract_State => The_Table
is
   --  The next two declarations are essentially the generic parameters.
   subtype Table_Component_Type is Integer;
   subtype Table_Index_Type     is Positive;

   Table_Low_Bound   : constant Table_Index_Type := Table_Index_Type'First;
   Table_Initial     : constant Positive := 8;
   Table_Increment   : constant Natural := 100;
   Release_Threshold : constant Natural := 0; -- size in bytes

   subtype Valid_Table_Index_Type is Table_Index_Type'Base
     range Table_Low_Bound .. Table_Index_Type'Base'Last;
   subtype Table_Last_Type is Table_Index_Type'Base
     range Table_Low_Bound - 1 .. Table_Index_Type'Base'Last;

   function First_Index return Table_Last_Type with
     Global =>The_Table,
     Post => First_Index'Result <= Valid_Table_Index_Type'Last;
   --  Returns the current value of the first used entry in the table, which
   --  can then be used as a subscript for Table.

   function Last_Index return Table_Last_Type with
     Global => The_Table,
     Post => Last_Index'Result <= Valid_Table_Index_Type'Last;
   --  Returns the current value of the last used entry in the table, which can
   --  then be used as a subscript for The_Table.

   function Is_Empty return Boolean is (Last_Index > First_Index) with
     Global => The_Table;

   procedure Init with
     Global => (Output => The_Table);
   --  Reinitializes the table to empty. There is no need to call this before
   --  using a table; tables default to empty.

   procedure Append (New_Val : Table_Component_Type) with
     Global => (In_Out => The_Table),
     Post => not Is_Empty and
                  (Last_Index = Last_Index'Old + 1) and
                  (Last_Index < Table_Last_Type'Last);
   --  Appends New_Val onto the end of the table
   --  Equivalent to:
   --    Increment_Last_Index;
   --    Set_Item (Last_Index) := New_Val;

   procedure Set_Item (Index : Valid_Table_Index_Type;
                       Item  : Table_Component_Type) with
     Global => (In_Out => Dynamic_Table.The_Table),
     Pre  => not Is_Empty,
     Post => not Is_Empty;
   --  Put Item in the table at position Index. If Index points to an existing
   --  item (i.e. it is in the range First .. Last_Index), the item is
   --  replaced. Otherwise (i.e. Index > Last_Index), the table is
   --  expanded, and Last_Index is set to Index.

   function Get_Item (Index : Valid_Table_Index_Type)
                      return Table_Component_Type with
     Global => The_Table,
     Pre => not Is_Empty;

   procedure Set_Last (New_Val : Table_Last_Type) with
     Global => (In_Out => The_Table),
     Post => Last_Index = New_Val;
   --  This procedure sets Last_Index to the indicated value. If necessary the
   --  table is reallocated to accommodate the new value (i.e. on return the
   --  allocated table has an upper bound of at least Last). If Set_Last
   --  reduces the size of the table, then logically entries are removed from
   --  the table. If Set_Last increases the size of the table, then new entries
   --  are logically added to the table.

end SPARK_2014.Dynamic_Table;

