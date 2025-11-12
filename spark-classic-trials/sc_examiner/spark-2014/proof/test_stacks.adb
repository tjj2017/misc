with Bounded_Stacks;
procedure Test_Stacks with
  SPARK_Mode
is
   package Stack is new Bounded_Stacks
     (Element_Type => Integer,
      Stack_Size   => 32);

   S : Stack.Stack;
begin
   Stack.New_Stack (S);
   pragma Assert (Stack.Is_Empty (S));
end Test_Stacks;
