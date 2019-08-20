-- with Text_IO; use Text_IO;
procedure Try_Range_Attribute (X : Integer) is
   I : constant Integer range 5 .. 10 := 5;
   type A_T is array (1 .. 10) of Integer;
   type A_U is array (Integer range <>) of Integer;
   type E_T is (Zero, One, Two, Three, Four, Five);
   A : constant A_T := (others => 7);
   B : constant A_U := (1, 2, 3, 4, 5, 6, 7, 8, 9, 0);
   subtype S_1 is Integer range Integer'Range;
   subtype S_2 is Integer range 11 .. 20;
   subtype S_3 is Integer range A_T'Range;
   subtype S_4 is Integer range A'Range (1);
   subtype S_5 is Integer range S_2'Range;
   subtype S_6 is S_2 range S_2'First+1 .. S_2'Last - 1;
   subtype S_7 is E_T range One .. Five;
   subtype S_8 is S_7 range S_7'Range;
   subtype S_9 is Integer range X + 1 .. X - 1;
--   subtype S_10 is Integer range S_7'Range;
--  subtype S_11 is Integer range B'Range;
begin

--     Put_Line ("S_1 Low bound is " & S_1'Image (S_1'First));
--     Put_Line ("S_1 High bound is " & S_1'Image (S_1'Last));
--     Put_Line ("S_3 Low bound is " & S_3'Image (S_3'First));
--     Put_Line ("S_3 High bound is " & S_3'Image (S_3'Last));
--     Put_Line ("S_4 Low bound is " & S_4'Image (S_4'First));
--     Put_Line ("S_4 High bound is " & S_4'Image (S_4'Last));

   pragma Assert (S_1'First = Integer'First);
   pragma Assert (S_1'Last = Integer'Last);
   pragma Assert (S_2'First = 11);
   pragma Assert (S_2'Last = 20);
   pragma Assert (S_3'First = 1);
   pragma Assert (S_3'Last = 10);
   pragma Assert (S_4'First = 1);
   pragma Assert (S_4'Last = 10);
   pragma Assert (S_5'First = 11);
   pragma Assert (S_5'Last = 20);
   pragma Assert (A (I) = 7);
end Try_Range_Attribute;
