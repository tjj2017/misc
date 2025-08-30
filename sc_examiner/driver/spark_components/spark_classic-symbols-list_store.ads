private package SPARK_Classic.Symbols.List_Store
--  --# own Store;
--  --# initializes Store;
with Abstract_State => (Store with Part_Of => Dictionary),
     Initializes    => (Store)
is
   procedure Append_Node (S_Node : Store_Node)
   --  --# global in out Store;
   with Global => (In_Out => Store);

   procedure Set_Node (N_List : Table_Ref; S_Node : Store_Node)
   --  --# global in out Store;
   with Global => (In_Out => Store);
end SPARK_Classic.Symbols.List_Store;
