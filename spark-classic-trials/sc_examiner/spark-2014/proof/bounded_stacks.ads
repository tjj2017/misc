generic
   type Element_Type is private;
   Stack_Size : Positive;

package Bounded_Stacks
with SPARK_Mode
is
   type Stack is private;

   subtype Stack_Count is Natural range 0 .. Stack_Size;
   subtype Stack_Index is natural range 1 .. Stack_Size;

   type Contents_Abstraction is array (Stack_Index) of Element_Type with
     Ghost;

   type Stack_Abstraction is
      record
         Contents  : Contents_Abstraction;
         Stack_Top : Stack_Count;
      end record with
     Ghost;

   function Stack_Abs (S : Stack) return Stack_Abstraction with
     Ghost;

   function Count    (S : Stack) return Stack_Count with
     Post => Count'Result = Stack_Abs (S).Stack_Top;

   function Is_Empty (S : Stack) return Boolean is (Count (S) = 0) with
   Post => Is_Empty'Result = (Stack_Abs (S).Stack_Top = 0);
   pragma Inline (Is_Empty);

   procedure New_Stack (S : out Stack)
     with Post => Is_Empty (S);

   pragma Unevaluated_Use_Of_Old (Allow);

   procedure Push (S : in out Stack;
                   Value : Element_Type) with
     Pre => Count (S) < Stack_Size,
     Post => not Is_Empty (S) and
             Count (S) = Count (S)'Old + 1 and
             Top (S) = Value and
             Stack_Abs (S).Stack_Top = Stack_Abs (S'Old).Stack_Top + 1 and
             Stack_Abs (S).Contents (Stack_Abs (S).Stack_Top) = Value and
             (for all P in Stack_Index range 1 .. Count (S)'Old =>
                Stack_Abs (S)'Old.Contents (P)  = Stack_Abs (S).Contents (P));

   procedure Pop  (S : in out Stack;
                   Value : out Element_Type) with
     Pre  => not Is_Empty (S),
     Post => Count (S) = Count (S)'Old - 1 and
             Value = Top (S'Old) and
             Stack_Abs (S).Stack_Top = (Stack_Abs (S'Old).Stack_Top - 1) and
             Value = Stack_Abs (S'Old).Contents (Stack_Abs (S'Old).Stack_Top)
             and
             (for all P in Stack_Index range 1 .. Count (S) =>
                Stack_Abs (S)'Old.Contents (P)  = Stack_Abs (S).Contents (P));


   function Top (S : Stack) return Element_Type with
     Pre  => not Is_Empty (S),
     Post => Top'Result = Stack_Abs (S).Contents (Stack_Abs (S).Stack_Top);

   function Predecessor (S : Stack;
                         Pred_Num : Stack_Count)
                         return Element_Type with
     Pre  => Pred_Num < Count (S),
     Post => Predecessor'Result = Stack_Abs (S).Contents (Pred_Num);

   procedure Clear (S : out Stack) with
     Post => Is_Empty (S) and Stack_Abs (S).Stack_Top = 0;

private
   type Stack_Contents is array (Stack_Index) of Element_Type;

   type Stack is
      record
         Count    : Stack_Count;
         Contents : Stack_Contents;
      end record;

end Bounded_Stacks;
