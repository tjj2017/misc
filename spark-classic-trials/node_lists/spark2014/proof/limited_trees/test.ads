pragma SPARK_Mode;
with SPARK_2014.Atrees;
--  with SPARK_2014.Trees;
--  with SPARK_2014.Bounded_Stacks;
package Test
  with SPARK_Mode
is
   package ATrees is new SPARK_2014.Atrees
     (Key_Type             => Positive,
      Value_Type           => Integer,
      Null_Value           => 0,
      Stack_Size           => 32);

   --  package Trees is new SPARK_2014.Trees
   --    (Key_Type             => Integer,
   --     Value_Type           => Integer,
   --     Null_Value           => 0);

   --  package Stacks is new SPARK_2014.Bounded_Stacks
   --    (Element_Type => Integer,
   --     Stack_Size   => 32);
end Test;
