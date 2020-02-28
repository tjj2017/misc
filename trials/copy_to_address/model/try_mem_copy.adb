with Buffers; use type Buffers.Smaller_Buf;
--  with Mem_Copy;
procedure Try_Mem_Copy is
--     A : Integer := 23;
--     B : Integer := 0;
   Local_B, Remote_B : Buffers.Smaller_Buf := (0, (others => 0));
begin
   Remote_B := Local_B;
   pragma Assert (Local_B = Remote_B);
   Local_B.Size := 1;
   Local_B.Data (1) := 1;
   pragma Assert (Local_B = Remote_B);
   Remote_B.Size := 1;
   Remote_B.Data (1) := 1;
   pragma Assert (Local_B = Remote_B);
end Try_Mem_Copy;
