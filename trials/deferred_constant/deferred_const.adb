package body Deferred_Const is
   function Where (N : Natural) return Priv is
      Result : Priv;
   begin
      if N < 5 then
	 Result := Empty;
      elsif N >5 and N < 10 then
	 Result := Half_Full;
      else
	 Result := Full;
      end if;
      return Result;
   end Where;
   
 end Deferred_Const;

