procedure Arr_First_Last_Len is
   type My_Int is range 0 .. 100;
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;
   type Arr_M is array (Index) of My_Int;
   type Arr_I is array (Index) of Index;
   type Arr_A is array (1 .. 4, 1 .. 4, 1 .. 4, 1 .. 4) of Integer;
   type Arr_B is array (Integer range 0 .. 5) of Integer;
   type Arr_U is array (Positive range <>) of Integer;
   subtype SU is Arr_U (Index);
   subtype SUN is Arr_U (1 .. 5);
   subtype SUS is Arr_U (Integer range 3 .. 4);
   subtype SSU is SU;
   subtype USU is Arr_U;
   subtype SA is Arr;

   type Arr_Of_Arr is array (5 .. 7) of Arr;
   type Arr_Of_Arrays is array (1 .. 2) of Arr_of_Arr;
   type Arr_Of_SU is array (Integer range <>) of SU;

   L : Integer := 2; U : Integer := 6;
   subtype Dyn_Arr is Arr_U (L .. U);

   type Constrained_Dyn is array (L .. U) of Integer;
   type CD_Arr_Arr is array (L .. U) of Arr;
   type CD_Arr_Arrays is array (L .. U) of Arr_Of_Arr;

   subtype Dyn_Sub is Integer range L .. U;
   subtype Dyn_Arr_S is Arr_U (Dyn_Sub);

   X, Y : Integer;

   procedure PC1 (A : Arr) is
   begin
      X := A'First;  -- Do_Array_First not called
   end PC1;

   procedure PC1_M (A : Arr_M) is
   begin
      X := A'First;  -- Do_Array_First not called
   end PC1_M;

   procedure PC1_I (A : Arr_I) is
   begin
      X := A'First;  -- Do_Array_First not called
   end PC1_I;

--     procedure PC2 (A : Arr_A) is
--        F : Integer;
--     begin
--        F := A'First (3);  -- Do_Array_First not called
--     end PC2;

--     procedure PC3 (A : Arr_Of_Arr) is
--        F : Integer;
--     begin
--        F := A'First;  -- Do_Array_First not called
--     end PC3;

--     procedure PC4 (A : Arr_Of_Arr) is
--        F : Integer;
--     begin
--        F := A (5)'First;  -- Do_Array_First not called
--     end PC4;

   procedure PC5 (A : Arr_U; F : out Integer) is
   begin
      F := A'First;  -- Invokes Do_Array_First with parent I_or_d constraint
                     -- Parent of parent is subtype_indication
--        **** Do_Array_First_Last_Length
--          N_Attribute_Reference (Node_Id=3264) (analyzed)
--          N_Range (Node_Id=3268) (analyzed)
--          N_Index_Or_Discriminant_Constraint (Node_Id=3277)
--          Subtype_Indication
--             N_Identifier "arr_u" (Node_Id=3361)
--             <empty>
--          N_Subtype_Indication (Node_Id=3278)
--          N_Identifier "a" (Node_Id=3263) (analyzed)
--          N_Attribute_Reference (Node_Id=3264) (analyzed)
--          N_Range (Node_Id=3268) (analyzed)
--          N_Defining_Identifier "a" (Entity_Id=3235)
--          N_Defining_Identifier "arr_u" (Entity_Id=2321) (source,analyzed)
--          N_Defining_Identifier "a" (Entity_Id=3235)
--          It's definitely an array
--        Is constrained subtype FALSE
--             Is constrained base type FALSE
--               N_Identifier "positive" (Node_Id=2328) (source)
--             N_Integer_Literal (Node_Id=1089s)
                     --             1
   end PC5;

   procedure PC6 (AA : Arr_of_Arr) is
      AAF : Integer;
   begin
      AAF := AA'First;  --  Does not invoke Do_Array_First
      AAF := AA (5)'First;  -- Does not invoke Do_Array_First
   end PC6;

   procedure PC7 (AA : Arr_Of_SU) is
      AAF : Integer;
   begin
      AAF := AA'First;  --  Invoke Do_Array_First
--        ******* First
--          N_Attribute_Reference (Node_Id=3306) (analyzed)
--          N_Range (Node_Id=3310) (analyzed)
--          N_Index_Or_Discriminant_Constraint (Node_Id=3319)
--          N_Subtype_Indication (Node_Id=3320)
--          Subtype_Indication
--             N_Identifier "arr_of_su" (Node_Id=3408)
--             <empty>
--          N_Identifier "aa" (Node_Id=3305) (analyzed)
--          N_Attribute_Reference (Node_Id=3306) (analyzed)
--          N_Range (Node_Id=3310) (analyzed)
--          N_Defining_Identifier "aa" (Entity_Id=3277)
--          N_Defining_Identifier "arr_of_su" (Entity_Id=2431) (source,analyzed)
--          N_Defining_Identifier "aa" (Entity_Id=3277)
--          It's definitely an array
--        Is constrained subtype FALSE
--             Is constrained base type FALSE
--               N_Identifier "integer" (Node_Id=2438) (source)
--           -2147483648

      AAF := SU'First;
      AAF := AA (5)'First;  -- Invokes Do_Array_First
--        ******* First
--          N_Attribute_Reference (Node_Id=3301) (analyzed)
--          N_Range (Node_Id=3305) (analyzed)
--          N_Index_Or_Discriminant_Constraint (Node_Id=3314)
--          N_Subtype_Indication (Node_Id=3315)
--          Subtype_Indication
--             N_Identifier "arr_of_su" (Node_Id=3408)
--             <empty>
--          N_Identifier "aa" (Node_Id=3300) (analyzed)
--          N_Attribute_Reference (Node_Id=3301) (analyzed)
--          N_Range (Node_Id=3305) (analyzed)
--          N_Defining_Identifier "aa" (Entity_Id=3272)
--          N_Defining_Identifier "arr_of_su" (Entity_Id=2431) (source,analyzed)
--          N_Defining_Identifier "aa" (Entity_Id=3272)
--          It's definitely an array
--        Is constrained subtype FALSE
--             Is constrained base type FALSE
--               N_Identifier "integer" (Node_Id=2438) (source)
--             N_Integer_Literal (Node_Id=980s)
--             -2147483648
      null;
   end PC7;

--     CA : Arr;
--     CB : Arr_Of_Arr (1 .. 5);

CU : Arr_U (Index);
--     CSUS : SUS;
--     CSA : SA;
--     CSU : SU;
--     CUSU : USU (7 .. 11);
--     T : Integer;
   AA : Arr_Of_Arr;
   AAA : Arr_Of_Arrays;
   CDAA : CD_Arr_Arr;
   CDAAA : CD_Arr_Arrays;
begin
   --  The following uses of attribute 'First are substituted by the
   --  front-end by their constant value;
   X := Arr'First;
   X := Arr_A'First;
   X := Arr_A'First (3);
   X := SU'First;
   --   X := "Arr_U'First;  --Front-end error "prefix must be constrained array"

   --  Dynamic arrays cause a problem the variable defining the lower bound
   --  is not directly kept by the front-end.
   --  It introduces a new node rewriting the original.
   X := Dyn_Arr'First;   -- Invokes call to Do_Array_First
--     ******* First
--       N_Attribute_Reference (Node_Id=2635) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2633) (source,analyzed)
--       N_Handled_Sequence_Of_Statements (Node_Id=2610) (source,analyzed)
--       N_Subprogram_Body (Node_Id=2253) (source,analyzed)
--       Not a subtype_indication
--       N_Identifier "dyn_arr" (Node_Id=2632) (source)
--       N_Attribute_Reference (Node_Id=2635) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2633) (source,analyzed)
--       N_Defining_Identifier "dyn_arr" (Entity_Id=2464) (source)
--       N_Defining_Identifier "dyn_arr" (Entity_Id=2464) (source)
--       N_Defining_Identifier "dyn_arr" (Entity_Id=2464) (source)
--       It's definitely an array
--     Is constrained subtype TRUE
--          Is constrained base type FALSE
--            N_Range (Node_Id=2474) (source,analyzed)
--          N_Identifier "R13b" (Node_Id=2473) (analyzed)

   X := Dyn_Arr_S'First; -- Invokes call to Do_Array_First
--     ******* First
--       N_Attribute_Reference (Node_Id=2661) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2659) (source,analyzed)
--       N_Handled_Sequence_Of_Statements (Node_Id=2636) (source,analyzed)
--       N_Subprogram_Body (Node_Id=2253) (source,analyzed)
--       Not a subtype_indication
--       N_Identifier "dyn_arr_s" (Node_Id=2658) (source)
--       N_Attribute_Reference (Node_Id=2661) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2659) (source,analyzed)
--       N_Defining_Identifier "dyn_arr_s" (Entity_Id=2537) (source)
--       N_Defining_Identifier "dyn_arr_s" (Entity_Id=2537) (source)
--       N_Defining_Identifier "dyn_arr_s" (Entity_Id=2537) (source)
--       It's definitely an array
--     Is constrained subtype TRUE
--          Is constrained base type FALSE
--            N_Identifier "dyn_sub" (Node_Id=2546) (source)
--          N_Identifier "dyn_sub_FIRST" (Node_Id=2532) (analyzed)
    -- A possible solution is to have first, last & length variable friends
    -- for unconstrained arrays and all their subtypes.

   --  The following uses of attribute 'First on arrays of arrays do not
   --  invoke Do_Array_First.
   X := Arr_Of_Arr'First;
   X := AA (5)'First;
   X := AAA'First;
   X := AAA (1)'First;
   X := AAA (1)(5)'First;
   X := CDAA (1)'First;
   X := CDAAA (1)'First;
   X := CDAAA (1)(5)'First;

--     X := Dyn_Sub'First; -- Invokes First_Last_Length but unsupported
--     ----------At: Do_First_Last_Length----------
--     ----------Dynamic subtypes are unsupported----------
--     N_Attribute_Reference (Node_Id=2717) (source,analyzed)
--       Parent = N_Assignment_Statement (Node_Id=2715)
--       Sloc = 16183  arr_first_last_len.adb:200:16
--         Etype = N_Defining_Identifier "integer" (Entity_Id=1009s)
--       Prefix = N_Identifier "dyn_sub" (Node_Id=2714)
--       Attribute_Name = "first" (Name_Id=300000830)

   X := Constrained_Dyn'First;  -- Invokes Do_Array_First
--     ******* First
--       N_Attribute_Reference (Node_Id=2691) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2689) (source,analyzed)
--       N_Handled_Sequence_Of_Statements (Node_Id=2610) (source,analyzed)
--       N_Subprogram_Body (Node_Id=2253) (source,analyzed)
--       Not a subtype_indication
--       N_Identifier "constrained_dyn" (Node_Id=2688) (source)
--       N_Attribute_Reference (Node_Id=2691) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2689) (source,analyzed)
--       N_Defining_Identifier "constrained_dyn" (Entity_Id=2477) (source,analyzed)
--       N_Defining_Identifier "constrained_dyn" (Entity_Id=2477) (source,analyzed)
--       N_Defining_Identifier "constrained_dyn" (Entity_Id=2477) (source,analyzed)
--       It's definitely an array
--     Is constrained subtype TRUE
--          Is constrained base type FALSE
--            N_Range (Node_Id=2487) (source,analyzed)
--          N_Identifier "R15b" (Node_Id=2484) (analyzed)
--
   X := CD_Arr_Arr'First;  -- Invokes Do_Array_First
--     ******* First
--       N_Attribute_Reference (Node_Id=2691) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2689) (source,analyzed)
--       N_Handled_Sequence_Of_Statements (Node_Id=2610) (source,analyzed)
--       N_Subprogram_Body (Node_Id=2253) (source,analyzed)
--       Not a subtype_indication
--       N_Identifier "cd_arr_arr" (Node_Id=2688) (source)
--       N_Attribute_Reference (Node_Id=2691) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2689) (source,analyzed)
--       N_Defining_Identifier "cd_arr_arr" (Entity_Id=2492) (source,analyzed)
--       N_Defining_Identifier "cd_arr_arr" (Entity_Id=2492) (source,analyzed)
--       N_Defining_Identifier "cd_arr_arr" (Entity_Id=2492) (source,analyzed)
--       It's definitely an array
--     Is constrained subtype TRUE
--          Is constrained base type FALSE
--            N_Range (Node_Id=2502) (source,analyzed)
--          N_Identifier "R19b" (Node_Id=2499) (analyzed)

       X := CDAAA'First;  -- Invokes Do_Array_First
--     ******* First
--       N_Attribute_Reference (Node_Id=2691) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2689) (source,analyzed)
--       N_Handled_Sequence_Of_Statements (Node_Id=2610) (source,analyzed)
--       N_Subprogram_Body (Node_Id=2253) (source,analyzed)
--       Not a subtype_indication
--       N_Identifier "cdaaa" (Node_Id=2688) (source,analyzed)
--       N_Attribute_Reference (Node_Id=2691) (source,analyzed)
--       N_Assignment_Statement (Node_Id=2689) (source,analyzed)
--       N_Defining_Identifier "cdaaa" (Entity_Id=2600) (source)
--       N_Defining_Identifier "cd_arr_arrays" (Entity_Id=2507) (source,analyzed)
--       N_Defining_Identifier "cdaaa" (Entity_Id=2600) (source)
--       It's definitely an array
--     Is constrained subtype TRUE
--          Is constrained base type FALSE
--            N_Range (Node_Id=2517) (source,analyzed)
--          N_Identifier "R23b" (Node_Id=2514) (analyzed)
   X := CU'First;
   PC5 (CU, Y);
   pragma Assert (X = Y);
--     PC5 ((1, 2, 3));


   --     Q (CB);
--     T := 1;
--
--     CA (T) := 23;
--  --   T := CA (1);
--     pragma Assert (CA (T) = X);
--  --     A := 1;
--  --     B := 10;
--  --     declare
--  --        type Dyn_Arr is array (A .. B) of Integer;
--  --     begin
--  --        null;
--  --     end;
   null;
end Arr_First_Last_Len;
