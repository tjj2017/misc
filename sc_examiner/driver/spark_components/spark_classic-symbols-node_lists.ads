with SPARK_Classic.Symbols.List_Store;
private package SPARK_Classic.Symbols.Node_Lists
is
   procedure New_List (N_List : out Table_Ref)
   --  --# global in out SPARK_Classic.Symbols.List_Store.Store;
   with Global => (In_Out => SPARK_Classic.Symbols.List_Store.Store);

   procedure Insert (N : Types.Node_Id; N_List : Table_Ref)
   --  --# global in out Store;
   with Global => (In_Out => SPARK_Classic.Symbols.List_Store.Store);

   procedure Save_List (N_List : in out Table_Ref)
   --  --# global in out Store;
   with Global => (In_Out => SPARK_Classic.Symbols.List_Store.Store);

   function Find (N : Types.Node_Id; N_List : Table_Ref) return Table_Ref
   --  --# global SPARK_Classic.Symbols.List_Store.Store;
   with Global => (Input => SPARK_Classic.Symbols.List_Store.Store);

end SPARK_Classic.Symbols.Node_Lists;
