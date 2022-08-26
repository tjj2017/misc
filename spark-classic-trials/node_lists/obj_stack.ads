with General_Object;
--# inherit General_Object;
package Obj_Stack is
   type Stack_Type is tagged private;

   subtype Nearly_Natural is Natural range 0 .. Natural'Last - 1;

   function Count    (S : Stack_Type) return Nearly_Natural;
   function Is_Empty (S : Stack_Type) return Boolean;
   --# return Is_Empty (S) = (Count (S) = 0);

   procedure Init (S : out Stack_Type);
   --# post Is_Empty (S);

   procedure Push (S : in out Stack_Type; V : General_Object.O);
   --# post Count (S) = Count (S~) + 1;

   procedure Pop  (S : in out Stack_Type; V : out General_Object.O);
   --# pre not Is_Empty (S);
   --# post Count (S) = Count (S~) - 1;

   function Top (S : Stack_Type) return General_Object.O;
    --# pre  not Is_Empty (S);

   function Predecessor (S : Stack_Type; Pred_Num : Nearly_Natural)
                         return General_Object.O;
    --# pre Pred_Num < Count (S);

private
   subtype Small_Positive is Positive range 1 .. 32;
   type Stack_Elements is array (Small_Positive) of General_Object.O;
   type Stack_Type is
     tagged record
      Contents : Stack_Elements;
      Count : Nearly_Natural;
   end record;
end Obj_Stack;
