with Interface_Module.Non_Dets;
package body Interface_Module is

   --------------
   -- Register --
   --------------

   procedure Register (Size : Positive; Addr : System.Address) is


   -----------------
   -- Read_Inputs --
   -----------------

   procedure Read_Inputs is
   begin
      Interface_Module.Non_Dets.Read (M1, M2, M3, M4);
   end Read_Inputs;

   -------------------
   -- Write_Outputs --
   -------------------

   procedure Write_Outputs is
   begin
      Interface_Module.Non_Dets.Write (M5, M6);
   end Write_Outputs;

   -----------
   -- Yield --
   -----------

   procedure Yield is
   begin
      null;
   end Yield;

end Interface_Module;
