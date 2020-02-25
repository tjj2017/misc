with Buffers;
with System;
with Interfaces; use Interfaces;
procedure Copy_From (B: System.Address; Addr : System.Address) is
   Remote_Buffer : Buffers.Max_Buf;
   for Remote_Buffer'Address use Addr;
   Local_Buffer : Buffers.Max_Buf;
   for Local_Buffer'Address use B;
begin
   Remote_Buffer.Size := Local_Buffer.Size;
   Remote_Buffer.Data (0 .. Local_Buffer.Size - 1) :=
     Local_Buffer.Data (0 .. Local_Buffer.Size - 1);
end Copy_From;
