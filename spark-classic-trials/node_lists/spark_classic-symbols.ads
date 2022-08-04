with Types;
package SPARK_Classic.Symbols
--  --# own Dictionary;
with Abstract_State => Dictionary
is
   pragma Elaborate_Body;
private
   --  A type for accessing elements of a table.
   --  Used to reference node lists, dependency relations and,
   --  if implemented, node list opitimisations.
   --  For convenience, the range of Node_Id's is used.

   type Extended_Table_Index is range
     Types.Node_Low_Bound .. Types.Node_High_Bound;

   No_Index : constant Extended_Table_Index := Extended_Table_Index'First;

   subtype Table_Index is Extended_Table_Index range
     Extended_Table_Index'Succ (Extended_Table_Index'First) ..
     Extended_Table_Index'Last;


--     --  Initial size of node lists table;
--     List_Store_Initial_Size : constant := 1_200;
--     --  Growth rate.
--     List_Store_Increment    : constant := 100;

end SPARK_Classic.Symbols;
