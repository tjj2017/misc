procedure Selectors is

   type Models is (Not_A_Model, Nondet, Nondet_In_Type, Represents);

   subtype Small_Int is Integer range 0 .. 10;

   subtype My_Positive is Integer range 1 .. Integer'Last;

   type R1 is record
      A : Integer;
      B : My_Positive;
   end record;

   type R2 is record
      A : My_Positive;
      B : Integer;
   end record;

   type R1_2 is record
      C1 : R1;
      C2 : R2;
   end record;

   VSI : Small_Int;
   VR1 : R1;
   VR2 : R2;
   VR1_2 : R1_2;

   procedure Nondet_In_Type_Vars
     with Global => (In_Out => (VSI, VR1, VR2, VR1_2)),
     Annotate => (ASVAT, Nondet_In_Type),
     Import;
begin
   VSI := 5;
   VR1.A := 23;
   VR1.B := 71;
   VR2.B := VR1.A;
   VR2.A := VR1.B;
   VR1_2.C1.A := VR1.A;
   VR1_2.C1.B := VR1.B;
   VR1_2.C2 := VR2;
   pragma Assert (VSI = 5);
   pragma Assert (VSI >= 0);
   pragma Assert (VR1.A = 23);
   pragma Assert (VR1.B = 71);
   pragma Assert (VR1.A in Integer);
   pragma Assert (VR1.B > 0);
   pragma Assert (VR2.A = 71);
   pragma Assert (VR2.B = 23);
   pragma Assert (VR2.A > 0);
   pragma Assert (VR2.B in Integer);

   Nondet_In_Type_Vars;

   pragma Assert (VSI = 5);
   pragma Assert (VSI >= 0);
   pragma Assert (VR1.A = 23);
   pragma Assert (VR1.B = 71);
   pragma Assert (VR1.A in Integer);
   pragma Assert (VR1.B >= Positive'First);
   pragma Assert (VR2.A = 71);
   pragma Assert (VR2.B = 23);
   pragma Assert (VR2.A > 0);
   pragma Assert (VR2.B in Integer);
end Selectors;
