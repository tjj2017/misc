--  This example is run with pragma Profile (Rational)
--  in the configuration file gnat.adc.
--
--  with Text_IO; use Text_IO;
procedure Iss_131 is
   type SI is range 0 .. 3;
   for SI'Size use 2;

   type OS is range 0 .. 6;
   for OS'Size use 16;

   VSI : SI := 1;
   VOS : OS := 5;

   VSIS : SI := 2;
   for VSIS'Size use 16;

   TYPE neighbours IS (tom, barbara, margo, jerry);
   TYPE neigh_array IS ARRAY (neighbours) OF boolean;
   FOR neigh_array'size use 4;

   Vna : neigh_array := (others => False);

   --  Declaration and initialisation causes cbmc to crash
   --  S : constant String := "Hello World";
   --  for S'Size use 88;

   type R1 is record
      A : Integer;
      B : Integer;
   end record;
   pragma Pack (R1);

   type R2 is record
      H : R1;
      X : Integer;
      Y : Integer;
      N : neigh_array;
   end record;
   pragma Pack (R2);

   type R3 is record
      H : R1;
      B1 : Boolean;
      B2 : Boolean;
   end record;
   pragma Pack (R3);
   for R3'Size use 66;

   type r is record
      a, b, c, d, e, f, g, h : boolean;
      chr                    : character;
   end record;
   for r'size use 16;

   type V1 (B : Boolean) is record
      case B is
         when True =>
            I : Integer;
         when False =>
            C : Character;
      end case;
   end record;
   pragma Pack (V1);
   for V1'Size use 40;

   type V2 (B : Boolean := False) is record
      case B is
         when True =>
            N : Natural;
         when False =>
            null;
      end case;
   end record;
   pragma Pack (V2);
   for V2'Size use 39;

   subtype STV1 is V1 (True);
   subtype SFV1 is V1 (False);

   VST : STV1;
   VSF : SFV1;

   VV2 : V2;

   VR1 : R1;
   VR2 : R2;
   VR3 : R3;

   VR : r;

   Test : Integer;

begin
--     Put_Line ("Natural size = " & Integer'Image (Natural'Size));
--     Put_Line ("VSI size " & Integer'Image (VSI'Size));
--     Put_Line ("VOS size " & Integer'Image (VOS'Size));
--     Put_Line ("VSIS size " & Integer'Image (VSIS'Size));
--     Put_Line ("neigh_array " & Integer'Image (neigh_array'Size));
--     Put_Line ("na component size " & Integer'Image (neigh_array'Component_Size));
--     Put_Line ("Vna size " & Integer'Image (Vna'Size));
--     Put_Line ("Vna component  size " & Integer'Image (Vna'Component_Size));
--     Put_Line ("Vna (barbara) size " & Integer'Image (Vna (barbara)'Size));
--     Put_Line ("neigh_array size " & Integer'Image (neigh_array'Size));
--     Put_Line ("neigh_array component size "
--               & Integer'Image (neigh_array'Component_Size));
--     Put_Line ("Vna (barbara..margo)'Size " &
--               Integer'Image (Vna (barbara .. margo)'Size));
--     Put_Line ("Vna (barbara..margo)'Component_Size " &
--               Integer'Image (Vna (barbara .. margo)'Component_Size));
--  --     Put_Line ("S size " & Integer'Image (S'Size));
--
--     Put_Line ("R1'Size " & Integer'Image (R1'Size));
--     Put_Line ("R2'Size " & Integer'Image (R2'Size));
--     Put_Line ("R3'Size " & Integer'Image (R3'Size));
--     Put_Line ("VR1'Size " & Integer'Image (VR1'Size));
--     Put_Line ("VR2'Size " & Integer'Image (VR2'Size));
--     Put_Line ("VR3'Size " & Integer'Image (VR3'Size));
--
--     Put_Line ("r'Size " & Integer'Image (r'Size));
--     Put_Line ("Vr'Size " & Integer'Image (Vr'Size));
--
--     Put_Line ("V1'Size " & Integer'Image (V1'Size));
--     Put_Line ("V2'Size " & Integer'Image (V2'Size));
--     Put_Line ("STV1'Size " & Integer'Image (STV1'Size));
--     Put_Line ("SFV1'Size " & Integer'Image (SFV1'Size));
--     Put_Line ("VST'Size " & Integer'Image (VST'Size));
--     Put_Line ("VSF'Size " & Integer'Image (VSF'Size));
--     Put_Line ("VV2'Size " & Integer'Image (VV2'Size));

   pragma Assert (Natural'Size = 32);
   pragma Assert (VSI'Size = 2);
   pragma Assert (VOS'size = 16);
   pragma Assert (VSIS'size = 16);
   pragma Assert (Vna'size = 4);
   pragma Assert (Vna (barbara)'size = 1);
   pragma Assert (neigh_array'size = 4);
   pragma Assert (Vna (barbara .. margo)'Size = 2);
   pragma Assert (Vna (barbara .. margo)'Component_Size = 1);
   --     pragma Assert (S'Size = 88);

   pragma Assert (R1'Size = 64);
   pragma Assert (R2'Size = 132);
   pragma Assert (R3'Size = 66);
   pragma Assert (VR1'Size = 64);
   pragma Assert (VR2'Size = 132);
   pragma Assert (VR3'Size = 66);
   pragma Assert (r'Size = 16);
   pragma Assert (Vr'Size = 16);

   pragma Assert (V1'Size = 64);
   pragma Assert (V2'Size = 39);
   pragma Assert (STV1'Size = 64);
   pragma Assert (SFV1'Size = 40);
   pragma Assert (VST'Size = 64);
   pragma Assert (VSF'Size = 40);
   pragma Assert (VV2'Size = 64);

   --  The front-end does not process the component size correctly with
   --  Profile (Rational) it substitutes the component_size with constant
   --  literal 8 in the AST but when its value is printed out, somehow,
   --  the back-end has changed the value to 1.
   --  Gnat2goto cannot correct this as it never
   --  encounters the attribute Component_Size in the AST.
   --  Hence the following 2 assertions are satisfied in the executable code
   --  but fail in cbmc.
   pragma Assert (Vna'Component_size = 1);
   pragma Assert (neigh_array'component_size = 1);

   --  If one looks at the goto function generated for this procedure,
   --  it can be seen that the front-end is putting the value 8 in the tree.
   Test := Vna'Component_Size;
   Test := neigh_array'Component_Size;
--     Test := S'Size;

   --  The following 2 assertions give a warning from the gnat compiler
   --  (but not gnat2goto) that they will fail at run time, but
   --  succeed in cbmc.
   pragma Assert (Vna'Component_size = 8);
   pragma Assert (neigh_array'component_size = 8);


end Iss_131;
