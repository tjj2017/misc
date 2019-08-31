pragma Ada_2012;
package body Inker is

   ---------
   -- Inc --
   ---------

   procedure Inc (X : in out Integer) is
   begin
      X := X + 1;
   end Inc;

end Inker;
