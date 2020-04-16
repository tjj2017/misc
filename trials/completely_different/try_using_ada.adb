with System;
with System.Address_To_Access_Conversions;
--  with Text_Io; use Text_IO;
procedure Try_Using_Ada is
   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;
   type Vector is array (Index) of Count;
   type Header is record
      I : Integer;
      Counter : Count;
   end record;

   generic
      type Object (<>) is limited private;

   package Local_Address_To_Access_Conversions is

      type Object_Pointer is access all Object;

      function To_Pointer (Value : System.Address) return Object_Pointer;
      function To_Address (Value : Object_Pointer) return System.Address;

   end Local_Address_To_Access_Conversions;


   package body Local_Address_To_Access_Conversions is
      package Intrinsic_Address_To_Access_Conversions is new
        System.Address_To_Access_Conversions (Object);

      function To_Pointer (Value : System.Address) return Object_Pointer is
      begin
         return Object_Pointer
           (Intrinsic_Address_To_Access_Conversions.To_Pointer (Value));
      end To_Pointer;

      function To_Address (Value : Object_Pointer) return System.Address is
      begin
         return Intrinsic_Address_To_Access_Conversions.To_Address
           (Intrinsic_Address_To_Access_Conversions.Object_Pointer (Value));
      end To_Address;

   end Local_Address_To_Access_Conversions;

   type Buffer_1 is record
      Head : Header;
      Vec  : Vector;
   end record;

   type Buffer_2 is record
      Head : Header;
      Vec_1  : Vector;
      Vec_2  : Vector;
   end record;

   procedure Update_1 (X : access Buffer_1) is
   begin
      X.Head.I := 23;
      X.Head.Counter := 5;

      for J in Index loop
         X.Vec (J) := J;
      end loop;
   end Update_1;

   procedure Update_2 (X : access Buffer_2) is
   begin
      X.Head.I := 13;
      X.Head.Counter := 7;

      for J in Index loop
         X.Vec_1 (J) := J; X.Vec_2 (J) := Index'Last - J + 1;
      end loop;
   end Update_2;

   procedure P_Add (A : System.Address; Which : Count) is
   begin
      case Which is
         when 1 =>
            declare
               package Buffer_1_Address_Access_Conversions is new
                 Local_Address_To_Access_Conversions (Buffer_1);
               use Buffer_1_Address_Access_Conversions;
            begin
               Update_1 (To_Pointer (A));
            end;
         when 2 =>
            declare
               package Buffer_2_Address_Access_Conversions is new
                 Local_Address_To_Access_Conversions (Buffer_2);
               use Buffer_2_Address_Access_Conversions;
            begin
               Update_2 (To_Pointer (A));
            end;
         when others =>
            null;
      end case;
   end P_Add;

   B_1 : Buffer_1;
   B_2 : Buffer_2;

begin
   P_Add (B_1'Address, 1);
   pragma Assert (B_1.Head.I = 23);
   pragma Assert (B_1.Head.Counter = 5);
   for J in Index loop
      pragma Assert (B_1.Vec (J) = J);
   end loop;

--     Put_Line ("I = " & Integer'Image (B_1.Head.I));
--     Put_Line ("Counter = " & Count'Image (B_1.Head.Counter));
--
--     Put_Line ("Vec =");
--     for J in B_1.Vec'Range loop
--        Put (Count'Image (B_1.Vec (J)));
--     end loop;
--     New_Line;

   P_Add (B_2'Address, 2);
   pragma Assert (B_2.Head.I = 13);
   pragma Assert (B_2.Head.Counter = 7);
   for J in Index loop
      pragma Assert (B_2.Vec_1 (J) = J);
      pragma Assert (B_2.Vec_2 (J) = Index'Last - J + 1);
   end loop;
--     Put_Line ("I = " & Integer'Image (B_2.Head.I));
--     Put_Line ("Counter = " & Count'Image (B_2.Head.Counter));
--
--     Put_Line ("Vec_1 =");
--     for J in B_2.Vec_1'Range loop
--        Put (Count'Image (B_2.Vec_1 (J)));
--     end loop;
--     New_Line;
--
--     Put_Line ("Vec_2 =");
--     for J in B_2.Vec_2'Range loop
--        Put (Count'Image (B_2.Vec_2 (J)));
--     end loop;
--     New_Line;
end Try_Using_Ada;
