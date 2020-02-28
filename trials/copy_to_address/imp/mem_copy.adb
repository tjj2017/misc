with System;
with Buffers;
procedure Mem_Copy (Destination, Source : System.Address;
                    No_Of_Bits : Natural)
--  with Annotate => (ASVAT, Memcpy)
is
   Source_Array, Destination_Array : Buffers.Byte_Array (1 .. No_Of_Bits / 8);
   for Source_Array'Address use Source;
   for Destination_Array'Address use Destination;
begin
   Destination_Array := Source_Array;
end Mem_Copy;
