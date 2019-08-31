package body Stacks_Monitoring is

   function Is_Empty(S : Monitored_Stack) return Boolean is
   begin
      return Stacks.Is_Empty (S.The_Stack);
   end Is_Empty;
   
   function Is_Full(S : Monitored_Stack) return Boolean is
   begin
      return Stacks.Is_Full (S.The_Stack);
   end Is_Full;
   
   procedure Clear(S : out Monitored_Stack) is
   begin
      Stacks.Clear (S.The_Stack);
      S.Next_Identity_Value := 1;
      S.Monitor_Vector := Monitor_Vector_T'(Index => 0);
      S.Monitor_Pointer := 0;
   end Clear;

   procedure Push(S : in out Monitored_Stack; X : in Integer) is
   begin
      Stacks.Push(S.The_Stack, X);
      S.Monitor_Pointer := S.Monitor_Pointer + 1;
      S.Monitor_Vector(S.Monitor_Pointer) := S.Next_Identity_Value;
      S.Next_Identity_Value := S.Next_Identity_Value + 1;
   end Push;
   
   procedure Pop(S : in out Monitored_Stack; X : out Integer) is
   begin
      Stacks.Pop (S.The_Stack, X);
      S.Monitor_Pointer := S.Monitor_Pointer - 1;
   end Pop;

   function Top_Identity(S : in Monitored_Stack) return Integer is
      Result : Integer;
   begin
      if Is_Empty(S) then
         Result := 0;
      else
         Result := S.Monitor_Vector(S.Monitor_Pointer);
      end if;
      return Result;
   end Top_Identity;

   function Next_Identity(S : in Monitored_Stack) return Integer is
   begin
      return S.Next_Identity_Value;
   end Next_Identity;

end Stacks_Monitoring;
