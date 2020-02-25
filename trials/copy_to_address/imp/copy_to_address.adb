with Buffers; use type Buffers.Smaller_Buf;
with Copy_From;
with Mem_Copy;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Text_IO; use Text_IO;
procedure Copy_To_Address is
   Local_B, Remote_B : Buffers.Smaller_Buf := (0, (others => 0));
   R : Uniformly_Distributed;
   G : Generator;
   N : Integer;
begin
   for Iters in 1 .. 5 loop
      R := Random (G);
      Put_Line ("R = " & Float'Image (R));
      N := Integer (R * Float (Buffers.Smaller_Buf_Size));
      Put_Line ("N = " & Integer'Image (N));
      Local_B.Size := N;

      for I in Buffers.Count'First .. Local_B.Size - 1 loop
         Local_B.Data (I) := Natural (Random (G) * Float (Natural'Last));
      end loop;

      Mem_Copy (Remote_B'Address,
                Local_B'Address,
                Local_B.Size'Size + Local_B.Size * Local_B.Data (0)'Size);

      Put_Line ("Local size  = " & Integer'Image (Local_B.Size));
      Put_Line ("Remote size = " & Integer'Image (Remote_B.Size));

      Put_Line ("Local data:");
      for I in Remote_B.Data'Range loop
         Put (Integer'Image (Local_B.Data (I)) & " ");
      end loop;
      New_Line;

      Put_Line ("Remote data:");
      for I in Remote_B.Data'Range loop
         Put (Integer'Image (Remote_B.Data (I)) & " ");
      end loop;
      New_Line;

      pragma Assert (Local_B = Remote_B);
   end loop;

end Copy_To_Address;
