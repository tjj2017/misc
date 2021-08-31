procedure Default_Init is
   type R is record
      I : Integer := 3;
      J : Integer;
   end record;
   VR : R;
begin
   pragma Assert (VR.I = 3);
   null;
end Default_Init;
