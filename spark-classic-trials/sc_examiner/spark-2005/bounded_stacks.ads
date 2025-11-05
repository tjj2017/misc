with Specific_Tree_Types;
with Basic_Tree;
--# inherit Specific_Tree_Types,
--#         Basic_Tree;
package Bounded_Stacks is
   Stack_Size : constant := Specific_Tree_Types.Stack_Size;
   subtype Element_Type is Basic_Tree.Valid_Node_Index;

   type Stack is private;

   subtype Stack_Count is Natural range 0 .. Stack_Size;

   function Count    (S : Stack) return Stack_Count;

   function Not_Empty (S : Stack) return Boolean;
   --# return Count (S) > Stack_Count'First;

   function Top (S : Stack) return Element_Type;
   --# pre Not_Empty (S);

   function Predecessor (S : Stack;
                         Pred_Num : Stack_Count)
                         return Element_Type;
   --# pre Pred_Num > Stack_Count'First and Pred_Num < Count (S);

    procedure New_Stack (S : out Stack);
   --# post Count (S) = Stack_Count'First;

   procedure Push (S : in out Stack; Value : Element_Type);
   --# pre Count (S) < Stack_Size;
   --# post Count (S) = Count (S~) + 1 and
   --#      Top (S) = Value;

   procedure Pop  (S : in out Stack; Value : out Element_Type);
   --# pre  Not_Empty (S);
   --# post Count (S) = Count (S~) - 1 and
   --#      Value = Top (S~);

   procedure Clear (S : out Stack);
   --# post Count (S) = Stack_Count'First;

private
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last;
   type Stack_Contents is array (Stack_Index) of Element_Type;

   type Stack is
      record
         Count    : Stack_Count;
         Contents : Stack_Contents;
      end record;

end Bounded_Stacks;
