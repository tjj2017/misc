with Pak; use Pak;
procedure P is
   Y : Integer := 1;
begin
   Inc (Y);
   pragma Assert (Y = 2);
end P;
