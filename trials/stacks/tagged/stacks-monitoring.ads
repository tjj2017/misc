--# inherit Stacks;
package Stacks.Monitoring is

   type Monitored_Stack is new Stacks.Stack with private;

   procedure Clear(S : out Monitored_Stack);
   --# derives S from ;

   procedure Push(S : in out Monitored_Stack; X : in Integer);
   --# derives S from S,X;

   function Top_Identity(S : in Monitored_Stack) return Integer;
   function Next_Identity(S : in Monitored_Stack) return Integer;

private

   type Monitored_Stack is new Stacks.Stack with
      record
         Monitor_Vector : Stacks.Vector;
         Next_Identity_Value : Integer;
      end record;

end Stacks.Monitoring;
