with System;
with System.Address_To_Access_Conversions;
with Text_Io; use Text_IO;
procedure Again_Try_Using_Ada is
   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;
   type Vector is array (Index) of Count;
   type Header is record
      I : Integer;
      Counter : Count;
   end record;

   generic
      type Object (<>) is limited private;

   package Address_To_Access is

      type Object_Pointer is access all Object;

      procedure Get_Access
        (Addr : System.Address; Obj : out Object_Pointer)
        with Annotate => (ASVAT, To_Pointer);
   end Address_To_Access;


   package body Address_To_Access is
      package Pointers is new
        System.Address_To_Access_Conversions (Object);
      use Pointers;

      procedure Get_Access
        (Addr : System.Address; Obj : out Object_Pointer) is
      begin
         Obj := Object_Pointer (To_Pointer (Addr));
      end Get_Access;
   end Address_To_Access;

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

      for J in X.Vec'Range loop
         X.Vec (J) := J;
      end loop;
   end Update_1;

   procedure Update_2 (X : access Buffer_2) is
   begin
      X.Head.I := 13;
      X.Head.Counter := 7;

      for J in Index'Range loop
         X.Vec_1 (J) := J; X.Vec_2 (J) := Index'Last - J + 1;
      end loop;
   end Update_2;

   procedure P_Add (A : System.Address; Which : Count) is
   begin
      case Which is
         when 1 =>
            declare
               package Get_Access_Buffer_1 is new
                 Address_To_Access (Buffer_1);
               use Get_Access_Buffer_1;
               type Obj_Pointer is access all Buffer_1;
               Obj : Obj_Pointer;
            begin
               Get_Access (A, Object_Pointer (Obj));
               Update_1 (Obj);
            end;
         when 2 =>
            declare
                package Get_Access_Buffer_2 is new
                 Address_To_Access (Buffer_2);
               use Get_Access_Buffer_2;
               type Obj_Pointer is access all Buffer_2;
              Obj : Obj_Pointer;
            begin
               Get_Access (A, Object_Pointer (Obj));
               Update_2 (Obj);
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
   for J in B_1.Vec'Range loop
      pragma Assert (B_1.Vec (J) = J);
   end loop;

   Put_Line ("I = " & Integer'Image (B_1.Head.I));
   Put_Line ("Counter = " & Count'Image (B_1.Head.Counter));

   Put_Line ("Vec =");
   for J in B_1.Vec'Range loop
      Put (Count'Image (B_1.Vec (J)));
   end loop;
   New_Line;

   P_Add (B_2'Address, 2);
   pragma Assert (B_2.Head.I = 13);
   pragma Assert (B_2.Head.Counter = 7);
   for J in Index'Range loop
      pragma Assert (B_2.Vec_1 (J) = J);
      pragma Assert (B_2.Vec_2 (J) = Index'Last - J + 1);
   end loop;
   Put_Line ("I = " & Integer'Image (B_2.Head.I));
   Put_Line ("Counter = " & Count'Image (B_2.Head.Counter));

   Put_Line ("Vec_1 =");
   for J in B_2.Vec_1'Range loop
      Put (Count'Image (B_2.Vec_1 (J)));
   end loop;
   New_Line;

   Put_Line ("Vec_2 =");
   for J in B_2.Vec_2'Range loop
      Put (Count'Image (B_2.Vec_2 (J)));
   end loop;
   New_Line;
end Again_Try_Using_Ada;
