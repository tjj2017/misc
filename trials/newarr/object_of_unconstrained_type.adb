procedure Object_Of_Unconstrained_Type is
   --  Type U_Array is an unconstrained array type.
   type U_Array is array (Integer range <>) of Integer;
   --  Subtype U_C_Array is a constrained subtype of U_Array.
   subtype U_C_Array is U_Array (1 .. 4);

   --  Constrained_Obj is an object of a constraied subtype
   --  Its bounds will be that specified in the constrained subtype, ie., 1,4.
   Constrained_Obj : U_C_Array := (1, 2, 3, 4);

   --  Unconstrained_Obj is an object with an unconstrained subtype.
   --  The object itself will be constrained by the inialization from a slice.
   --  The slice is constrained and wil have the bounds 1, 3 and these are the
   --  bounds taken by Unconstrained_Obj.
   --  Auxilliary variables Unconstrained_Object___First_1 and
   --  Unconstrained_Object___Last_1 are introduced in the goto code to
   --  represent the upper and lower bounds of the object.
   --  The value of Unconstrained_Obj will be initialized from the slice of
   --  Constrained_Obj and is {1, 2, 3}.  The assignment is compatible because
   --  U_C_Array is a subtype of U_Array.
   Unconstrained_Obj : U_Array := Constrained_Obj (1 .. 3);
begin
   for I in Unconstrained_Obj'Range loop
      pragma Assert (Unconstrained_Obj (I) = I);
   end loop;

   null;
end Object_Of_Unconstrained_Type;
