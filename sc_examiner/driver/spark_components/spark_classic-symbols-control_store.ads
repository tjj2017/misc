with Table;
private package SPARK_Classic.Symbols.Control_Store is new Table.Table
  (Table_Component_Type => Table_Ref,
   Table_Index_Type     => Table_Ref,
   Table_Low_Bound      => Table_Ref'First,
   Table_Initial        => Control_Store_Initial_Size,
   Table_Increment      => Control_Store_Increment,
   Table_Name           => "Control_Store");
