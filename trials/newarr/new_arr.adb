--  with Text_IO; use Text_IO;
procedure New_Arr is
--     A, B : Integer;
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;
   type Arr_A is array (1 .. 10, 1 .. 10, 1 .. 10, 1 .. 10) of Integer;
   type Arr_B is array (Integer range 0 .. 5) of Integer;
     type Arr_U is array (Positive range <>) of Integer;
     subtype SU is Arr_U (Index);
     subtype SUN is Arr_U (1 .. 5);
   subtype SUS is Arr_U (Integer range 3 .. 4);
   subtype SSU is SU;
   subtype USU is Arr_U;
   subtype SA is Arr;

   type Arr_Of_Arr is array (Integer range <>) of Arr;

--     procedure P (A : Arr_U) is
--        F, L, Len : Integer;
--     begin
--        F := A'First;
--        L := A'Last;
--        Len := A'Length;
--     end P;

   procedure Q (AA : Arr_of_Arr) is
      AAF : Integer;
   begin
      AAF := AA (1)'First;
--        Put_Line (Integer'Image (AAF));
   end Q;


   CA : Arr;
   CB : Arr_Of_Arr (1 .. 5);
   CU : Arr_U (Index);
   CSUS : SUS;
   CSA : SA;
   CSU : SU;
   CUSU : USU (7 .. 11);
   T : Integer;
   X : Integer;
begin
   Q (CB);
   T := 1;

   CA (T) := 23;
--   T := CA (1);
   pragma Assert (CA (T) = 23);
--     A := 1;
--     B := 10;
--     declare
--        type Dyn_Arr is array (A .. B) of Integer;
--     begin
--        null;
--     end;
   null;
end New_Arr;
