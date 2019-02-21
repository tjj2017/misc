

package body T1Q2
is

  procedure Increment2 (X,Y: in out Integer)
  is
  begin
    X := X + 1;
    --# assert true;
    Y := Y + 1;
  end Increment2;

end T1Q2;
