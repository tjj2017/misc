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
     Pre  => not Is_Empty (S) and then Pred_Num < Count (S),
     Inline;

   function Top (S : Stack) return Element_Type with
     Pre  => not Is_Empty (S),
     Post => Top'Result = Predecessor (S, 0),
     Inline;

   --  Proof function stating the stack contents up to the stack with the
   --  lowest count are preserved (are the same).
   function Preserved (S1, S2 : Stack) return Boolean is
     (if Count (S1) = Count (S2) then
          (for all P in Stack_Count range 0 .. Count (S1) - 1 =>
                  Predecessor (S1, P) = Predecessor (S2, P))
      elsif Count (S1) < Count (S2) then
          (for all P in Stack_Count range 0 .. Count (S1) - 1 =>
                  Predecessor (S1, P) = Predecessor (S2, P + 1))
      else
           (for all P in Stack_Count range 0 .. Count (S2) - 1 =>
                  Predecessor (S1, P + 1) = Predecessor (S2, P))) with
     Pre  => not Is_Empty (S1) and not Is_Empty (S2),
     Ghost;

   procedure New_Stack (S : out Stack) with
   Post => Is_Empty (S);

   procedure Push (S : in out Stack; Value : Element_Type) with
     Pre => Count (S) < Stack_Size,
     Post => Count (S) = Count (S'Old) + 1 and Count (S) > 0 and
             not Is_Empty (S) and
             Top (S) = Value and
             (if not Is_Empty (S'Old) then Preserved (S'Old, S)),
     Inline;

   procedure Pop  (S : in out Stack; Value : out Element_Type) with
     Pre  => not Is_Empty (S),
     Post => Count (S) = Count (S'Old) - 1 and
             Value = Top (S'Old) and
             (if not Is_Empty (S) then Preserved (S'Old, S)),
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
