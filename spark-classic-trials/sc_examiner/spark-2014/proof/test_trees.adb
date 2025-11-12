with Basic_Tree;
procedure Test_Trees with
  SPARK_Mode
is
   type Node_Index is range 0 .. 10;

  package Tree is new Basic_Tree
     (Node_Index => Node_Index,
      Level_Type => Natural,
      Key_Type   => Natural,
      Value_Type => Integer,
      Null_Key   => 0,
      Null_Value => -1);

   T : Tree.Tree;
begin
   Tree.Init (T);
   pragma Assert (Tree.Empty_Tree (T));
end Test_Trees;
