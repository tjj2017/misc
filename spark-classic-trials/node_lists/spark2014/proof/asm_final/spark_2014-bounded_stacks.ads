generic
   type Element_Type is private;
   Stack_Size : Positive;

package SPARK_2014.Bounded_Stacks
with SPARK_Mode
is
   type Stack is private;

   subtype Stack_Count is Natural range 0 .. Stack_Size;

   function Count    (S : Stack) return Stack_Count;
   function Is_Empty (S : Stack) return Boolean is (Count (S) = 0);
   pragma Inline (Is_Empty);

   procedure New_Stack (S : out Stack)
     with Post => Is_Empty (S);

   procedure Push (S : in out Stack;
                   Value : Element_Type)
     with Pre => Count (S) < Stack_Size,
          Post => not Is_Empty (S) and
                  Count (S) = Count (S)'Old + 1 and
                  Top (S) = Value;

   procedure Pop  (S : in out Stack;
                   Value : out Element_Type)
     with Pre => not Is_Empty (S),
     Post => Count (S) = Count (S)'Old - 1;

   function Top (S : Stack) return Element_Type
     with Pre => not Is_Empty (S);

   function Predecessor (S : Stack;
                         Pred_Num : Stack_Count)
                         return Element_Type
     with Pre => Pred_Num < Count (S);

   procedure Clear (S : out Stack)
     with Post => Is_Empty (S);

private
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last;
   type Stack_Contents is array (Stack_Index) of Element_Type;

   type Stack is
      record
         Count    : Stack_Count;
         Contents : Stack_Contents;
      end record;

end SPARK_2014.Bounded_Stacks;
