with Inker;
with Deker;
procedure Ink_Dek is
   How_Many : constant := 10;
   Z : Integer := 0;
   Count : Integer := 0;
begin
   while Z /= How_Many loop
      Inker.Inc (Z);
      Count := Count + 1;
   end loop;

   pragma Assert (Z = How_Many);
   pragma Assert (Count = Z);

   while Z /= 0 loop
      Deker.Dec (Z);
   end loop;

   pragma Assert (Z = 0);
   -- This assertion should fail
   pragma Assert (Count = Z);
end Ink_Dek;
