generic
   type Element_Type is private;
   Stack_Size : Positive;

package Bounded_Stacks
with SPARK_Mode
is
   type Stack is private;

   subtype Stack_Count is Natural range 0 .. Stack_Size;

   function Count    (S : Stack) return Stack_Count with
   Inline;

   function Is_Empty (S : Stack) return Boolean is (Count (S) = 0) with
   Inline;

   function Predecessor (S : Stack;
                         Pred_Num : Stack_Count)
                         return Element_Type with
     Pre  => Pred_Num < Count (S),
     Inline;

   function Top (S : Stack) return Element_Type with
     Pre  => not Is_Empty (S),
     Post => Top'Result = Predecessor (S, 0),
     Inline;

   procedure New_Stack (S : out Stack) with
   Post => Is_Empty (S);

   procedure Push (S : in out Stack; Value : Element_Type) with
     Pre => Count (S) < Stack_Size,
     Post => not Is_Empty (S) and
             Count (S) = Count (S'Old) + 1 and
             Top (S) = Value and
             (for all P in Stack_Count range 0 .. Count (S'Old) =>
                  Predecessor (S, P) = Predecessor (S'Old, P + 1)),
     Inline;

   procedure Pop  (S : in out Stack; Value : out Element_Type) with
     Pre  => not Is_Empty (S),
     Post => Count (S) = Count (S'Old) - 1 and
             Value = Top (S'Old) and
             (for all P in Stack_Count range 1 .. Count (S) =>
                  Predecessor (S, P - 1)  = Predecessor (S'Old, P)),
       Inline;


   procedure Clear (S : out Stack) with
     Post => Is_Empty (S);

private
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last;

   type Stack_Contents is array (Stack_Index) of Element_Type;

   type Stack is
      record
         Count    : Stack_Count;
         Contents : Stack_Contents;
      end record;

end Bounded_Stacks;
