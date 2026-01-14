pragma Ada_2022;
with Atrees;
procedure Test_Atree with
  SPARK_Mode
is
   package Atree is new Atrees
     (Key_Type          => Natural,
      Value_Type        => Integer,
      Null_Key          => 0,
      Null_Value        => 0,
      Max_Nodes_In_Tree => 64,
      Stack_Size        => 32);

   A : Atree.A_Tree;
   H : Atree.Host_Tree;
begin
   Atree.Init_Host_Tree (H);
   Atree.New_A_Tree (A, H);
   pragma Assert (not Atree.Populated (A, H));
end Test_Atree;
