with GNAT.Table;
private package SPARK_Classic.Symbols.Node_List_DB is new GNAT.Table
  (Table_Component_Type => Store_Node,
   Table_Index_Type     => Table_Index,
   Table_Low_Bound      => Table_Index'First,
   Table_Initial        => List_Store_Initial_Size,
   Table_Increment      => List_Store_Initial_Size);
