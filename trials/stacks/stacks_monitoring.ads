with Stacks;
--# inherit Stacks;
package Stacks_Monitoring is

   type Monitored_Stack is private;

   function Is_Empty(S : Monitored_Stack) return Boolean;
   function Is_Full(S : Monitored_Stack) return Boolean;

   procedure Clear(S : out Monitored_Stack);
   --# derives S from ;

   procedure Push(S : in out Monitored_Stack; X : in Integer);
   --# derives S from S,X;
   
   procedure Pop(S : in out Monitored_Stack; X : out Integer);
   --# derives S, X from S;

   function Top_Identity(S : in Monitored_Stack) return Integer;
   function Next_Identity(S : in Monitored_Stack) return Integer;

private

   type Pointer_Range is range 0 .. Stacks.Stack_Size;
   subtype Index is Pointer_Range range 
     Pointer_Range'First + 1 .. Pointer_Range'Last;
   type Monitor_Vector_T is array (Index) of Integer;
   
   type Monitored_Stack is
      record
	 The_Stack : Stacks.Stack;
         Monitor_Vector : Monitor_Vector_T;
         Next_Identity_Value : Integer;
         Monitor_Pointer : Pointer_Range;
      end record;

end Stacks_Monitoring;
