package body T1Q4
is

   procedure ISQRT(N: in Natural; Root: out Natural)
   is
      -- Introduce a new subtype to use to avoid possible overflow in the
      -- evaluation of the expression in loop exit statement.
      subtype Big_Natural is Long_Long_Integer range 0..Long_Long_Integer'Last;

      Local_Root : Big_Natural;

   begin
      Local_Root := 0;

      loop
         exit when (Local_Root + 1) * (Local_Root + 1) > Big_Natural (N);
         -- Loop Invariant
         --# assert (Local_Root + 1) * (Local_Root + 1) <= Big_Natural(N) and
         --#        Local_Root <= Big_Natural(N);

         Local_Root := Local_Root + 1;

       -- Move the loop invariant to here instead and modify as necessary

      end loop;

      Root := Natural(Local_Root);

   end ISQRT;
end T1Q4;
