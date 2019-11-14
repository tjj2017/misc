with System;
package Interface_Module is

   type Time is mod 2**32;

   No_Of_Message_Sorts : constant := 5;
   type Message_No_T is range 0 .. No_Of_Message_Sorts;

   type Message_Header is
      record
         Count : Natural;
         Message_Number : Message_No_T;
         Message_Time : Time;
      end record;

   type M1_T is
      record
         Header : Message_Header;
         X : Integer;
         Y : Integer;
      end record;

   type M2_T is
      record
         Header : Message_Header;
         X : Integer;
         Y : Boolean;
      end record;

   type M3_T is
      record
         Header : Message_Header;
         X : Integer;
         Y : Integer;
         Z : Integer;
      end record;

   type M4_T is
      record
         Header : Message_Header;
         X : Integer;
         Y : Character;
         Z : Positive;
      end record;

   type M5_T is
      record
         Header : Message_Header;
         X : Integer;
         Y : Integer;
         Z : Natural;
      end record;

   type M6_T is
      record
         Header : Message_Header;
         X : Integer;
         Y : Integer;
         Z : Natural;
   end record;

   --  Inputs

   M1 : M1_T;
   M2 : M2_T;
   M3 : M3_T;
   M4 : M4_T;

   --  Outputs
   M5 : M5_T;
   M6 : M6_T;

   procedure Register (Size : Positive; Addr : System.Address);

   procedure Read_Inputs;
   procedure Write_Outputs;
   procedure Yield;

end Interface_Module;
