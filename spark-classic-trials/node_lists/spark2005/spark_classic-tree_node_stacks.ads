with SPARK_Classic.Bounded_Stacks;
with SPARK_Classic.Node_Id_Trees;
package SPARK_Classic.Tree_Node_Stacks is new
  Bounded_Stacks (Element_Type => Node_Id_Trees.Tree_Node,
                  Stack_Size   => 32);
