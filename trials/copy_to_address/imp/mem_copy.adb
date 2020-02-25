with System;
with Buffers;
procedure Mem_Copy (Destination, Source : System.Address; Size : Natural) is
   Source_Array, Destination_Array : Buffers.Byte_Array (1 .. Size / 8);
   for Source_Array'Address use Source;
   for Destination_Array'Address use Destination;
begin
   Destination_Array := Source_Array;
end Mem_Copy;
