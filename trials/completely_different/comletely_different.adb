procedure Comletely_Different is
   subtype Small is Integer range 0 .. 10;
   generic
      type Discrete is (<>);
   package ASVAT_Models is
      function Nondet return Discrete;
      function In_Type (V : Discrete) return Boolean;

      generic
         type Index   is (<>);
         type Discrete_Vector is array (Index) of Discrete;
      procedure Nondet_Vector (DV : out Discrete_Vector);

      generic
         type Index   is (<>);
         type Discrete_Vector is array (Index) of Discrete;
      procedure In_Type_Vector (DV : in Discrete_Vector);

   end ASVAT_Models;

   package body ASVAT_Models is
      function Nondet return Discrete is
         Nondet_Var : Discrete;
      begin
         pragma Warnings (Off, Nondet_Var);
         return Nondet_Var;
      end Nondet;

      function In_Type (V : Discrete) return Boolean is (V in Discrete);

      procedure Nondet_Vector (DV : out Discrete_Vector) is
      begin
         for I in Index loop
            DV (I) := Nondet;
         end loop;
      end Nondet_Vector;

      procedure In_Type_Vector (DV : in Discrete_Vector) is
      begin
         for I in Index loop
           pragma Assume (In_Type (DV (I)));
         end loop;
      end In_Type_Vector;

   end ASVAT_Models;


   package Models is new ASVAT_Models (Discrete => Small);
   use Models;

   type Small_Array is array (Small) of Small;

   procedure Nondet is new Nondet_Vector
     (Index           => Small,
      Discrete_Vector => Small_Array);

   procedure In_Type is new In_Type_Vector
     (Index           => Small,
      Discrete_Vector => Small_Array);

   S : Small := 5;
   SA : Small_Array := (others => 7);
begin
   pragma Assert (S = 5);
   pragma Assert (S in Small);

   S := Nondet;

   pragma Assert (S = 5);
   pragma Assert (S in Small);

   pragma Assume (In_Type (S));

   pragma Assert (S = 5);
   pragma Assert (S in Small);

   for I in SA'Range loop pragma Assert (SA (I) = 7); end loop;
   for I in SA'Range loop pragma Assert (SA (I) in Small); end loop;

   Nondet (SA);

   for I in SA'Range loop pragma Assert (SA (I) = 7); end loop;
   for I in SA'Range loop pragma Assert (SA (I) in Small); end loop;

   In_Type (SA);

   for I in SA'Range loop pragma Assert (SA (I) = 7); end loop;
   for I in SA'Range loop pragma Assert (SA (I) in Small); end loop;


end Comletely_Different;
