with SPARK_Classic.Symbols.Node_List_DB;
private package SPARK_Classic.Symbols.Nodes is
   procedure New_List (N_List : out Table_Index)
   --  --# global in out SPARK_Classic.Symbols.Node_List_DB.Table,
   --  --#               SPARK_Classic.Symbols.Node_List_DB.Locked;
     with Global => (In_Out => (SPARK_Classic.Symbols.Node_List_DB.Table,
                                SPARK_Classic.Symbols.Node_List_DB.Locked));

   procedure Insert (N : Types.Node_Id; N_List : Table_Index)
   --  --# global in out SPARK_Classic.Symbols.Node_List_DB.Table,
   --  --#               SPARK_Classic.Symbols.Node_List_DB.Locked;
   with Global => (In_Out => (SPARK_Classic.Symbols.Node_List_DB.Table,
                              SPARK_Classic.Symbols.Node_List_DB.Locked));

   procedure Save_List (N_List : in out Table_Index)
   --  --# global in out SPARK_Classic.Symbols.Node_List_DB.Table,
   --  --#               SPARK_Classic.Symbols.Node_List_DB.Locked;
   with Global => (In_Out => (SPARK_Classic.Symbols.Node_List_DB.Table,
                              SPARK_Classic.Symbols.Node_List_DB.Locked));

   function Find (N : Types.Node_Id; N_List : Table_Index) return Table_Index
   --  --# global in SPARK_Classic.Symbols.Node_List_DB.Table;
     with Global => (Input => SPARK_Classic.Symbols.Node_List_DB.Table);

end SPARK_Classic.Symbols.Nodes;
