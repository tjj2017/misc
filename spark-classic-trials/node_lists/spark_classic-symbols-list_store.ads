--  --# aquire GNAT.Dynamic_Tables;
private package SPARK_Classic.Symbols.List_Store
--  --# own Store;
--  --# initializes Store;
with Abstract_State => (Store with Part_Of => Dictionary),
     Initializes    => Store
is
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

   procedure Append (S_Node : Store_Node)
   --  --# global in out Store;
     with Global => (In_Out => Store);

   function Get_Item (Index : Table_Index) return Store_Node
   --  --# global in Store;
   with Global => (Input => Store);

   function Last return Table_Index
   -- --# global Store;
   with Global => (Input => Store);

   procedure Set_Item (N_List: Table_Index; Item : Store_Node)
   --  --# global in Store;
   with Global => (In_Out => Store);

end SPARK_Classic.Symbols.List_Store;
