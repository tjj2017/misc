package Declared_In_Subprog
--# own V;
is
   V : Integer;
   procedure P (X : in out Integer);
   --# global out V;
end Declared_In_Subprog;
