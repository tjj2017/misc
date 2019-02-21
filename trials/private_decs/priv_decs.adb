pragma Ada_2012;
package body Priv_Decs is

   ----------------
   -- Accumulate --
   ----------------

   procedure Accumulate (X : Natural) is
   begin
      Accum := Accum + X;
   end Accumulate;

end Priv_Decs;
