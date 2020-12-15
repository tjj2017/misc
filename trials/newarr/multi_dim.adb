with Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
procedure Multi_Dim is
   No_Of_Dim : constant := 3;
   type Arr is array (0 .. 26) of Integer;
   type N_Dim is array (0..2, 0..3, 0 .. 2) of integer;

--     function Unchecked_To_Arr is new Unchecked_Conversion (Source => N_Dim,
--                                                            Target => Arr);
--     function Unchecked_To_N_Dim is new Unchecked_Conversion (Source => Arr,
--                                                              Target => N_Dim);
   type Indices is array (1 .. No_Of_Dim) of Integer;
   type Dim_Size is array (1 .. No_Of_Dim) of Integer;

   function Calc_Offset (Ix : Indices; DimSiz : Dim_Size) return Natural is
      Offset : Natural := Ix (1);
   begin
      for I in 2 .. DimSiz'Last loop
         Offset := DimSiz (I) * Offset + Ix (I);
      end loop;
      return Offset;
   end Calc_Offset;

--     Mat : N_Dim; --  := ((1, 2, 3), (4, 5, 6, 7), (8, 9, 10));
--     A : Arr;
   Index : constant Indices := (0, 1, 2);
   Dims  : constant Dim_Size := (3, 4, 3);
begin
--     A := Unchecked_To_Arr (Mat);
--     for I in Arr'Range loop
--        Put (Integer'Image (A (I)) & " ");
--     end loop;
--     New_Line;
--
--     A := (10, 11, 12, 13, 14, 15, 16, 17, 18);
--     Mat := Unchecked_To_N_Dim (A);
--
--     for I in Mat'Range (1) loop
--        for J in Mat'Range (2) loop
--           Put (Integer'Image (Mat (I, J)) & " ");
--        end loop;
--     end loop;
--     New_Line;

   for I in N_Dim'Range (1) loop
      for J in N_Dim'Range (2) loop
         for K in N_Dim'Range (3) loop
            Put_Line ("The Offset for index (" & Integer'Image (I) &
              ", " & Integer'Image (J) &
              ", " & Integer'Image (K) &
              ") = " &
               Integer'Image (Calc_Offset ((I, J, K), Dims)));
         end loop;
      end loop;
   end loop;

end Multi_Dim;
