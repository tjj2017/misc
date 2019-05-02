pragma SPARK_Mode;
procedure t1q1paa (X : in out Integer) is
   Old_X : constant Integer := X;
begin
   pragma Assume (X < Integer'Last);
   X := X + 1;
   pragma Assert (X = Old_X + 1);
end t1q1paa;
