with System;
package Memory_Copy is
   procedure Mem_Copy (Destination, Source : System.Address;
                       No_Of_Bytes : Natural)
     with Annotate => (ASVAT, Memcpy),
          Import   => True;
end Memory_Copy;
