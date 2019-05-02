procedure Main is
	Dummy : Integer := 41;
begin
	Dummy := Dummy + 1;
	pragma Assert (Dummy = 42);
end Main;
