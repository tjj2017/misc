with System; use type System.Address;
package Memory_Copy is
   procedure Mem_Copy (Destination, Source : System.Address;
                       No_Of_Bits : Natural)
     with Pre      => No_Of_Bits Mod 8 = 0,
          Annotate => (ASVAT, Memcpy),
          Import   => True;

   function Another (Destination, Source : System.Address;
                     No_Of_Bits : Natural) return Boolean is
      (No_Of_Bits mod 8 = 0 and Destination /= Source);
end Memory_Copy;
