with Types;
--# inherit Types;
package SPARK_Classic.Expose_Nodes is
   type Tree_Node is range 0 .. Natural'Last - 1;
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   Empty_Node : constant Tree_Node := 0;

   subtype Key_Type is Types.Node_Id;
   type Value_Type is (No_Value);

   Null_Value : constant Value_Type := No_Value;

   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Natural;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record;
end SPARK_Classic.Expose_Nodes;
