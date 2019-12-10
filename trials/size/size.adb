procedure Size is
   type My_Int is range 0 .. 255;
   for My_Int'Size use 8;
   I : Integer;
   J : My_Int;
   for J'Size use 32;
   type A is array (Positive range <>) of Integer;
   subtype B is A (1 .. 10);
   VB : constant B := (others => 0);
   VA : A := (1,2,3,4,5);
   type Def_A is array (1 .. 10) of Integer;
   VDA : constant Def_A := (others => 0);
   type R is record
      A, B : Integer;
   end record;
   pragma Pack (R);

   type RD is record
      A : Def_A;
   end record;
   pragma Pack (RD);

   VR : R := (A | B => 0);
   VRD : RD := (A => (others => 0));

   My_String : constant String := "Hello World";

begin
   I := I'Size;
   J := J'Size;
   J := R'Size;
   J := RD'Size;
   J := VR'Size;
   J := VRD'Size;

   I := B'Size;
   I := VA'Size;
   I := Def_A'Size;
   I := VDA'Size;
   I := My_String'Size;
   I := A'Size;
end Size;
