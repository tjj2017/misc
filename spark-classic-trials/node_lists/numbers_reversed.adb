with General_Object,
     Obj_Stack;
--# inherit General_Object,
--#         Obj_Stack;
procedure Numbers_Reversed is
   subtype Index is Positive range 1 .. 10;
   type Num_Array is array (Index) of Integer;
   type Element_Type is new General_Object.O with
      record
         Value : Integer;
      end record;
   Nums : constant Num_Array := Num_Array'
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   Stack : Obj_Stack.Stack_Type;

   El : Element_Type;
begin
   Stack.Init;
   for I in Index loop
      El.Value := Nums (I);
      Stack.Push (El);
   end loop;

   for I in Index loop
      Stack.Pop (El);
      pragma Assert (El.Value = Nums (Index - I + 1));
   end loop;
end Numbers_Reversed;
