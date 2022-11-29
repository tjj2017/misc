with Types;
with SPARK_Classic.Null_Record;
with SPARK_Classic.Trees;
package SPARK_Classic.Node_Id_Trees is new
  SPARK_Classic.Trees
    (Key_Type   => Types.Node_Id,
     Value_Type => Null_Record.Null_Type,
     Null_Value => Null_Record.Null_Value);
