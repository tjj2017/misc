package Array_Decs is
   subtype Index is Integer range 1 .. 10;
   subtype Small is Integer range 0 .. 63;

   type Fixed_Array is array (Index) of Small;

   procedure Nondet_In_Type_Fixed (A : out Fixed_Array)
     with Global   => null,
          Annotate => (ASVAT, Nondet_In_Type),
          Import   => True;
end Array_Decs;
