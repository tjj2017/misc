with General_Object;
--# inherit General_Object;
package Stacks_Of_O is
   type Stack_Element is new General_Object.O with
      record
         Value : Natural;
      end record;
end Stacks_Of_O;
