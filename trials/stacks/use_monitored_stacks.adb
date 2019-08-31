with Stacks_Monitoring;
--# inherit Stacks_Monitoring;
--# main_program;
procedure Use_Monitored_Stacks is
   S : Stacks_Monitoring.Monitored_Stack;
   Count : Natural := 0;
   Val : Integer := 0;
   Top_Val : Integer;
begin
   Stacks_Monitoring.Clear (S);
   pragma Assert (Stacks_Monitoring.Is_Empty (S));
   while not Stacks_Monitoring.Is_Full (S) loop
      Count := Count + 1;
      Stacks_Monitoring.Push (S, Count);
   end loop;
   
   pragma Assert (Stacks_Monitoring.Is_Full (S));
   pragma Assert (Stacks_Monitoring.Top_Identity (S) < Integer'Last);
   
   while not Stacks_Monitoring.Is_Empty (S) loop
      Stacks_Monitoring.Pop (S, Top_Val);
      Val := Val + Top_Val;
   end loop;
   
   pragma Assert (Stacks_Monitoring.Is_Empty (S));
   pragma Assert (Val in 1 .. Integer'Last);
end Use_Monitored_Stacks;
