pragma SPARK_Mode;
--  with SPARK_2014.Atrees;
with SPARK_2014.Trees;
--  with SPARK_2014.Bounded_Stacks;
--  with SPARK_2014.Dynamic_Tables;
package Test
  with SPARK_Mode
is
   --  package ATrees is new SPARK_2014.Atrees
   --    (Key_Type             => Positive,
   --     Value_Type           => Integer,
   --     Null_Value           => 0,
   --     Stack_Size           => 32);

   package Trees is new SPARK_2014.Trees
     (Key_Type             => Integer,
      Value_Type           => Integer,
      Null_Value           => 0);

   --  package Stacks is new SPARK_2014.Bounded_Stacks
   --    (Element_Type => Integer,
   --     Stack_Size   => 32);

   --  package Tables is new SPARK_2014.Dynamic_Tables
   --    (Table_Component_Type => Integer,
   --     Table_Index_Type     => Positive,
   --     Table_Low_Bound      => 1,
   --     Table_Initial        => 32,
   --     Table_Increment      => 100);
end Test;
