--# inherit SPARK_2005.Multi_ATree;
private package SPARK_2005.Multi_ATree.Stack_Ops is

   function Count    (S : Multi_ATree.Stack) return Multi_Atree.Stack_Count;

   function Is_Empty (S : Multi_ATree.Stack) return Boolean;
   --# return Count (S) = 0;
   pragma Inline (Is_Empty);

   procedure New_Stack (S : out  Multi_ATree.Stack);
   --# post Is_Empty (S);

   function Top (S :  Multi_ATree.Stack) return Multi_ATree.Valid_Tree_Node;
   --# pre not Is_Empty (S);

   procedure Push (S : in out  Multi_ATree.Stack;
                   Value : Multi_ATree.Valid_Tree_Node);
   --# pre Count (S) <  Multi_ATree.Stack_Size;
   --# post not Is_Empty (S) and
   --#        Count (S) = Count (S~) + 1 and Top (S) = Value;

   procedure Pop  (S : in out Multi_ATree.Stack;
                   Value : out Multi_ATree.Valid_Tree_Node);
   --# pre  not Is_Empty (S);
   --# post Count (S) = Count (S~) - 1;

   function Predecessor (S : Multi_ATree.Stack;
                         Pred_Num : Multi_Atree.Stack_Index)
                         return Multi_ATree.Valid_Tree_Node;
   --# pre Pred_Num < Count (S);

   procedure Clear (S : out Multi_ATree.Stack);
   --# post Is_Empty (S);

end SPARK_2005.Multi_Atree.Stack_Ops;
