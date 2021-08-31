procedure SortStuff is
   type Index is range 1 .. 10;
   type List is array (Index) of Integer;
   PROCEDURE sort (l : IN OUT list) IS
   BEGIN
      FOR outer_index IN index'succ (l'first) .. l'last LOOP
         FOR inner_index IN REVERSE outer_index .. l'last LOOP
            null;
         END LOOP;
      END LOOP;
   END sort;
begin
   null;
end SortStuff;
