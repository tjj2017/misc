procedure Very_Simple is
   X : Integer := Integer'Last;
begin
   pragma Assert (X + 1 < Integer'Last);
   X := X + 1;
end Very_Simple;

