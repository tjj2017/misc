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

   type Table_Index is range Types.Node_Low_Bound .. Types.Node_High_Bound;

   No_Index : constant Table_Index := Table_Index'First;

  type Store_Node is
      record
         Level : Natural;
         Left  : Table_Index;
         Right : Table_Index;
         Value : Types.Node_Id;
      end record;

   Null_Node : constant Store_Node := Store_Node'
     (Level => 0,
      Left  => No_Index,
      Right => No_Index,
      Value => Types.Empty);

--     --  Initial size of node lists table;
--     List_Store_Initial_Size : constant := 1_200;
--     --  Growth rate.
--     List_Store_Increment    : constant := 100;

end SPARK_Classic.Symbols;
