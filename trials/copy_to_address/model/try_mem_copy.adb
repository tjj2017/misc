--  with Mem_Copy;
--  with Buffers; use type Buffers.Smaller_Buf;
with Memory_Copy; use Memory_Copy;
procedure Try_Mem_Copy is
   A : Integer := 23;
   B : Integer := 0;
   C : Integer := 5;
   D : Integer := 7;

   Temp : Boolean;
--   Local_B : Buffers.Smaller_Buf := (0, (others => 0));
--   Remote_B : Buffers.Smaller_Buf;
begin
   pragma Assert (A = 23);
   pragma Assert (B = 0);
   pragma Assert (B = 23);
   pragma Assert (A'Size mod 8 = 0);
   Mem_Copy (B'Address, A'Address, A'Size);
   Temp := Another (B'Address, A'Address, A'Size);
--   pragma Assert (A = 23);
--   pragma Assert (B = 0);
   D := B;
--   pragma Assert (D = 23);
--   pragma Assert (B = 23);
--     Remote_B := Local_B;
--     pragma Assert (Local_B = Remote_B);
--     Local_B.Size := 1;
--     Local_B.Data (1) := 1;
--     pragma Assert (Local_B = Remote_B);
--     Remote_B.Size := 1;
--     Remote_B.Data (1) := 1;
--     pragma Assert (Local_B = Remote_B);
--     Local_B.Size := Local_B.Data'Length / 2;
--     Local_B.Data (Local_B.Size - 1) := 3;
--     pragma Assert (Local_B = Remote_B);
--     Remote_B.Size := Local_B.Size;
--     Remote_B.Data (Remote_B.Size - 1) := 3;
--     pragma Assert (Local_B = Remote_B);
--     Local_B.Size := Local_B.Data'Length;
--     Local_B.Data (Local_B.Size - 1) := 5;
--     pragma Assert (Local_B = Remote_B);
--     Remote_B.Size := Local_B.Size;
--     Remote_B.Data (Remote_B.Size - 1) := 5;
--      pragma Assert (Local_B = Remote_B);
--
   null;
end Try_Mem_Copy;
