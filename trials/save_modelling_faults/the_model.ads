with Visible_Types;
with Visible_Vars;
package The_Model is
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

   procedure Update_Nondet_Visible_Anno
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
 --                               Visible_Vars.Var_My_Real,
   --                             Visible_Vars.Var_My_Fixed,
                                Visible_Vars.Var_My_Enum,
--                                Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
--                                Visible_Vars.Var_My_Sub_Real,
   --                             Visible_Vars.Var_My_Sub_Fixed,
                                Visible_Vars.Var_My_Sub_Enum)),
     Annotate => (ASVAT, Nondet),
     Import => True;

   procedure Update_Nondet_Visible_Import
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
--                                Visible_Vars.Var_My_Real,
    --                            Visible_Vars.Var_My_Fixed,
                                Visible_Vars.Var_My_Enum,
--                                Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
--                                Visible_Vars.Var_My_Sub_Real,
--                          Visible_Vars.Var_My_Sub_Fixed,
                                Visible_Vars.Var_My_Sub_Enum));
   pragma Import (Ada, Update_Nondet_Visible_Import, "Nondet");

   procedure Update_Nondet_In_Type_Visible_Anno
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
 --                               Visible_Vars.Var_My_Real,
   --                             Visible_Vars.Var_My_Fixed,
                                Visible_Vars.Var_My_Enum,
--                                Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
--                                Visible_Vars.Var_My_Sub_Real,
   --                             Visible_Vars.Var_My_Sub_Fixed,
                                Visible_Vars.Var_My_Sub_Enum)),
     Annotate => (ASVAT, Nondet_In_Type),
     Import => True;

   procedure Update_Nondet_In_Type_Visible_Import
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
--                                Visible_Vars.Var_My_Real,
    --                            Visible_Vars.Var_My_Fixed,
                                Visible_Vars.Var_My_Enum,
--                                Visible_Vars.Var_Sub_Int,
                                Visible_Vars.Var_My_Sub_Int,
--                                Visible_Vars.Var_My_Sub_Real,
--                          Visible_Vars.Var_My_Sub_Fixed,
                                Visible_Vars.Var_My_Sub_Enum));
   pragma Import (Ada, Update_Nondet_In_Type_Visible_Import, "Nondet_In_Type");

end The_Model;
