pragma Ada_2012;
package body Pak is

   procedure Inc (X : in out Integer) is
      Old_X : Integer := X;
   begin
      X := X + 1;
      pragma Assert (X = Old_X + 1);
   end Inc;

end Pak;
