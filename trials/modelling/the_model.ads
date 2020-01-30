with Visible_Types; --  Note the hidden types are not with'd or visible
with Visible_Vars;  --  Note the hidden vars are not with'd and not visible
package The_Model is
   --  Declare local variables to represent the hidden ones

   --  First use annotation
   Hidden_Var_Int : Integer
     with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_Int");

   --  Now use Import pragma
   Hidden_Var_My_Int : Visible_Types.My_Int;
   pragma Import (Ada, Hidden_Var_My_Int,
                  "Represents", "Hidden_Vars.Var_My_Int");

   Hidden_Var_Sub_Int : Visible_Types.Sub_Int
     with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_Sub_Int");

   Hidden_Var_My_Sub_Int : Visible_Types.My_Sub_Int;
   pragma Import (Ada, Hidden_Var_My_Sub_Int,
                  "Represents", "Hidden_Vars.Var_My_Sub_Int");


   --  Declare local types and variables to represent the hidden ones
   type My_Int is range -1024 .. 1023;
   subtype My_Sub_Int is My_Int range 0 .. 10;

   subtype Int is Integer;
   subtype Sub_Int is Int range 23 ..29;

   Hidden_Types_And_Vars_Var_Int : Int
     with Annotate => (ASVAT, Represents,
                 "Hidden_Types_And_Vars.Var_Int:Hidden_Types_And_Vars.Int");
   Hidden_Types_And_Vars_Var_My_Int : My_Int;

   Hidden_Types_And_Vars_Var_Sub_Int : Sub_Int;
   Hidden_Types_And_Vars_Var_My_Sub_Int : My_Sub_Int;


--     subtype Co_Ord is Integer range 0 .. 360;
--
--     To_Be_Updated : Integer
--       with Annotate => (ASVAT, Represents, "Try_New_Pragma.To_Be_Updated");
--     X : Co_Ord;
--     Y : Co_Ord;
--

--     pragma Import (Ada,
--                    Entity     => To_Be_Updated,
--                    External_Name => "Represents",
--                    Link_Name => "Try_New_Pragma.To_Be_Updated");
--     pragma Import (Ada, X, "Represents",
--                    "Try_New_Pragma.X:Try_New_Pragma.Co_Ord");
--     pragma Import (Ada, Y, "Represents",
--                    "Try_New_Pragma.Y:Try_New_Pragma.Co_Ord");

   procedure Update_Nondet_Anno
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                                Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
                                Hidden_Var_Int,
                                Hidden_Var_My_Int,
                                Hidden_Var_Sub_Int,
                                Hidden_Var_My_Sub_Int)),
     Annotate => (ASVAT, Nondet),
     Import => True;

   procedure Update_Nondet_Import
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                               Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
                                Hidden_Var_Int,
                                Hidden_Var_My_Int,
                                Hidden_Var_Sub_Int,
                                Hidden_Var_My_Sub_Int));
   pragma Import (Ada, Update_Nondet_Import, "Nondet");

   procedure Update_Nondet_In_Type_Anno
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                                Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
                                Hidden_Var_Int,
                                Hidden_Var_My_Int,
                                Hidden_Var_Sub_Int,
                                Hidden_Var_My_Sub_Int)),
     Annotate => (ASVAT, Nondet_In_Type),
     Import => True;

   procedure Update_Nondet_In_Type_Import
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                               Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
                                Hidden_Var_Int,
                                Hidden_Var_My_Int,
                                Hidden_Var_Sub_Int,
                                Hidden_Var_My_Sub_Int));
   pragma Import (Ada, Update_Nondet_In_Type_Import, "Nondet_In_Type");

end The_Model;
