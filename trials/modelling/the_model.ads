with Visible_Types;
with Visible_Vars;
package The_Model is
   --  Local variables declared to represent hidden variables
   Var_Int : Integer
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_Int");
   Var_My_Int : Visible_Types.My_Int
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_My_Int");
   Var_My_Enum : Visible_Types.My_Enum
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_My_Enum");
   Var_My_Sub_Int : Visible_Types.My_Sub_Int
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_My_Sub_Int");
   Var_My_Sub_Enum : Visible_Types.My_Sub_Enum;
   pragma Import (Ada,
                  Entity        => Var_My_Sub_Enum,
                  External_Name => "Represents",
                  Link_Name     => "Hidden_Vars.Var_My_Sub_Enum");

   --  Local Types and variables to represent the non-visible ones
   --  in package Hidden_Types
   type My_Int is range -1024 .. 1023;
   type My_Enum is (one, two, three, four, five);

   subtype My_Sub_Int is My_Int range 0 .. 10;
   subtype My_Sub_Enum is My_Enum range two .. four;

   Hid_Int : Integer
   with Annotate => (ASVAT, Represents,
                     "Hidden_Types.Hid_Int");
   Hid_My_Int : My_Int
   with Annotate => (ASVAT, Represents,
                     "Hidden_Types.Hid_My_Int",
                     "Hidden_Types.My_Int");
   Hid_My_Enum : My_Enum
   with Annotate => (ASVAT, Represents,
                     "Hidden_Types.Hid_My_Enum",
                     "Hidden_Types.My_Enum");
   Hid_My_Sub_Int : My_Sub_Int
   with Annotate => (ASVAT, Represents,
                     "Hidden_Types.Hid_My_Sub_Int",
                     "Hidden_Types.My_Sub_Int");
   Hid_My_Sub_Enum : My_Sub_Enum;
   pragma Import (Ada,
                  Entity        => Hid_My_Sub_Enum,
                  External_Name => "Represents",
                  Link_Name     =>
                    "Hidden_Types.Hid_My_Sub_Enum:Hidden_Types.My_Sub_Enum");

   procedure Update_Nondet_Anno
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                                Visible_Vars.Var_My_Enum,
                                Visible_Vars.Var_My_Sub_Int,
                                Visible_Vars.Var_My_Sub_Enum,
                                --  Hidden variables
                                Var_Int,
                                Var_My_Int,
                                Var_My_Enum,
                                Var_My_Sub_Int,
                                Var_My_Sub_Enum,
                                --  Hidden types and vars
                                Hid_Int,
                                Hid_My_Int,
                                Hid_My_Enum,
                                Hid_My_Sub_Int,
                                Hid_My_Sub_Enum)),
     Annotate => (ASVAT, Nondet),
     Import => True;

   procedure Update_Nondet_Import
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                                Visible_Vars.Var_My_Enum,
                                Visible_Vars.Var_My_Sub_Int,
                                Visible_Vars.Var_My_Sub_Enum,
                                --  Hidden variables
                                Var_Int,
                                Var_My_Int,
                                Var_My_Enum,
                                Var_My_Sub_Int,
                                Var_My_Sub_Enum,
                                --  Hidden types and vars
                                Hid_Int,
                                Hid_My_Int,
                                Hid_My_Enum,
                                Hid_My_Sub_Int,
                                Hid_My_Sub_Enum));
   pragma Import (Ada, Update_Nondet_Import, "Nondet");

   procedure Update_Nondet_In_Type_Anno
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                                Visible_Vars.Var_My_Enum,
                                Visible_Vars.Var_My_Sub_Int,
                                Visible_Vars.Var_My_Sub_Enum,
                                --  Hidden variables
                                Var_Int,
                                Var_My_Int,
                                Var_My_Enum,
                                Var_My_Sub_Int,
                                Var_My_Sub_Enum,
                                --  Hidden types and vars
                                Hid_Int,
                                Hid_My_Int,
                                Hid_My_Enum,
                                Hid_My_Sub_Int,
                                Hid_My_Sub_Enum)),
     Annotate => (ASVAT, Nondet_In_Type),
     Import => True;

   procedure Update_Nondet_In_Type_Import
     with Global => (In_Out => (Visible_Vars.Var_Int,
                                Visible_Vars.Var_My_Int,
                                Visible_Vars.Var_My_Enum,
                                Visible_Vars.Var_My_Sub_Int,
                                Visible_Vars.Var_My_Sub_Enum,
                                --  Hidden variables
                                Var_Int,
                                Var_My_Int,
                                Var_My_Enum,
                                Var_My_Sub_Int,
                                Var_My_Sub_Enum,
                                --  Hidden types and vars
                                Hid_Int,
                                Hid_My_Int,
                                Hid_My_Enum,
                                Hid_My_Sub_Int,
                                Hid_My_Sub_Enum));
   pragma Import (Ada, Update_Nondet_In_Type_Import, "Nondet_In_Type");

end The_Model;
