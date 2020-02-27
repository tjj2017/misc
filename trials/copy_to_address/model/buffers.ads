package Buffers is
   Max_Count : constant Positive := 20;
   subtype Count is Integer range 0 .. Max_Count;
   subtype Index is Count range Count'First .. Count'Last - 1;
   type A is array (Index range <>) of Integer;

   type Max_Buf is record
      Size : Count;
      Data : A (Index);
   end record;

   Smaller_Buf_Size : constant Positive := 10;
   type Smaller_Buf is record
      Size : Count;
      Data : A (Index'First .. Smaller_Buf_Size - 1);
   end record;

   type Byte is mod 256;

   type Byte_Array is array (Positive range <>) of Byte;
end Buffers;
