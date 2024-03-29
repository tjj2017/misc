generic
   type Element_Type is private;
   Stack_Size : Positive;

package SPARK_Classic.Bounded_Stacks is
   type Stack is tagged private;

   subtype Stack_Count is Natural range 0 .. Stack_Size;

   function Count    (S : Stack) return Stack_Count;
   function Is_Empty (S : Stack) return Boolean
     with Post => Is_Empty'Result = (Count (S) = 0);
   --  --# return Count (S) = 0;

   procedure New_Stack (S : out Stack)
     with Post => Is_Empty (S);
   --  --# post Is_Empty (S);

   procedure Push (S : in out Stack;
                   Value : Element_Type)
     with Pre => Count (S) < Stack_Size,
          Post => Count (S) = Count (S'Old) + 1;
   --  --# pre  Count (S) < Stack_Size;
   --  --# post Count (S) = Count (S~) + 1;

   procedure Pop  (S : in out Stack;
                   Value : out Element_Type)
     with Pre  => not Is_Empty (S),
     Post => Count (S) = Count (S'Old) - 1;
   --  --# pre not Is_Empty (S);
   --  --# post Count (S) = Count (S~) - 1;

   function Top (S : Stack) return Element_Type
     with Pre => not Is_Empty (S);
   --  --# pre  not Is_Empty (S);

   function Predecessor (S : Stack;
                         Pred_Num : Stack_Count)
                         return Element_Type
     with Pre => Pred_Num < Count (S);
   --  --# pre Pred_Num < Count (S);

   procedure Clear (S : out Stack)
     with Post => Is_Empty (S);
   --  --# post Is_Empty (S);
private
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last;
   type Stack_Contents is array (Stack_Index) of Element_Type;

   type Stack is tagged
      record
         Count    : Stack_Count;
         Contents : Stack_Contents;
      end record;

end SPARK_Classic.Bounded_Stacks;
