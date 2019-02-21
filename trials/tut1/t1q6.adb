

package body T1Q6
is

  procedure Raise_To_Power(X: in Integer; Y: in Natural; Z: out Integer)
  is
    A, C: Integer;
    B: Natural;
  begin
    A := X;
    B := Y;
    C := 1;
    while B > 0 loop
      if B mod 2 = 0 then
        B := B / 2;
        A := A * A;
      else
        B := B - 1;
        C := C * A;
      end if;
    end loop;
    Z := C;
  end Raise_To_Power;

end T1Q6;

