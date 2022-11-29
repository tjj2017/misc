with GNAT.Dynamic_Tables;
generic
   type Table_Component_Type is private;
   type Max_Table_Range is range <>;
   Initial_Size    : Positive;
   Table_Increment : Positive;
package SPARK_Classic.Dynamic_Table is
   type Index_Type is private;
   type Table_Type is tagged private;

   procedure Init (T : out Table_Type);

   procedure Append (T : in out Table_Type; I : out Index_Type);

   function Index (T : Table_Type; I : Index_Type) return Table_Component_Type;

   procedure Set (T : in out Table_Type;
                  I : Index_Type;
                  V : Table_Component_Type);
private
   type Index_Type is new Max_Table_Range;

   package Dynamic_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Table_Component_Type,
      Table_Index_Type     => Index_Type,
      Table_Low_Bound      => Index_Type'First,
      Table_Initial        => Initial_Size,
      Table_Increment      => Table_Increment);

   type Table_Type is tagged
      record
         Table : Dynamic_Tables.Instance;
      end record;
end SPARK_Classic.Dynamic_Table;
