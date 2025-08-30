pragma SPARK_Mode;
with Types;            use Types;
with Atrees;
package Abs_Int is
   package An_Atree is new Atrees
     (Atree_Node => Node_Id,
      Key_Type   => Node_Id,
      Value_Type => Natural,
      Null_Key   => Empty,
      Null_Value => 0,
      Stack_Size => 32);
end Abs_Int;
