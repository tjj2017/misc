with System;
procedure Mem_Copy (Destination, Source : System.Address;
                    No_Of_Bytes : Natural)
  with Annotate => (ASVAT, Memcpy)
is
begin
   null;
end Mem_Copy;
