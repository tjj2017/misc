package Specific_Tree_Types is
   Node_Low_Bound : constant := 0;
   --  The tree Id values start at zero, because we use zero for Empty (to
   --  allow a zero test for Empty). Actual tree node subscripts start at 0
   --  since Empty is a legitimate node value.

   Node_High_Bound : constant := 099_999_999;
   --  Maximum number of nodes that can be allocated is 100 million, which
   --  is in practice infinite, and there is no need to check the range.

   type Tree_Node is range Node_Low_Bound .. Node_High_Bound; -- type range <>;
   subtype Valid_Tree_Node is Tree_Node range
     Tree_Node'Succ (Tree_Node'First) .. Tree_Node'Last;
   subtype Level_Type is Natural;         -- type range <>;
   subtype Key_Type is Tree_Node;         -- type (<>);
   subtype Value_Type is Integer;         -- type private;
   Null_Key : constant Key_Type := 0;
   Null_Value : constant Value_Type := 0;
   Stack_Size : constant Positive := 32;
end Specific_Tree_Types;
