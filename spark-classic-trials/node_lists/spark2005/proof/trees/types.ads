package Types is

   Node_Low_Bound : constant := 0;
   --  The tree Id values start at zero, because we use zero for Empty (to
   --  allow a zero test for Empty). Actual tree node subscripts start at 0
   --  since Empty is a legitimate node value.

   Node_High_Bound : constant := 100; --  099_999_999;
   --  Maximum number of nodes that can be allocated is 100 million, which
   --  is in practice infinite, and there is no need to check the range.

    type Node_Id is range Node_Low_Bound .. Node_High_Bound;
   --  Type used to identify nodes in the tree

   Empty : constant Node_Id := Node_Id'First;

end Types;
