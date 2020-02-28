with System;
procedure Mem_Copy (Destination, Source : System.Address;
                    No_Of_Bits : Natural)
  with Pre      => No_Of_Bits mod 8 = 0,
       Annotate => (ASVAT, Memcpy)
is
begin
   null;
end Mem_Copy;
