with Mem_Copy;
procedure Try_Mem_Copy is
   A : Integer := 23;
   B : Integer := 0;
begin
   Mem_Copy (B'Address, A'Address, A'Size);
end Try_Mem_Copy;
