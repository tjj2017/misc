with Visible_Types; --  Note the hidden types are not with'd or visible
with Visible_Vars;  --  Note the hidden vars are not with'd and not visible
use type Visible_Types.My_Int;
package The_Model is
   --  Local variables declared to represent hidden variables.
   --  First using ASVAT annotation.
   Var_Int : Integer
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_Int");
   Var_My_Int : Visible_Types.My_Int
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_My_Int");
   Var_My_Enum : Visible_Types.My_Enum
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_My_Enum");
   Var_My_Sub_Int : Visible_Types.My_Sub_Int
   with Annotate => (ASVAT, Represents, "Hidden_Vars.Var_My_Sub_Int");
   -- An example of using pragma Import to indicate that this local variable
   --  represents a non-visible variable.
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

   --  First use the ASVAT annotation to indicate that this local variable
   --  and its local type declaration represent a non-visible variable and type.
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
   --  Similarly, using pragma Import. Not that the type is separated from the
   --  variable using a ":" and that no spaces are allowed.
   Hid_My_Sub_Enum : My_Sub_Enum;
   pragma Import (Ada,
                  Entity        => Hid_My_Sub_Enum,
                  External_Name => "Represents",
                  Link_Name     =>
                    "Hidden_Types.Hid_My_Sub_Enum:Hidden_Types.My_Sub_Enum");

   --  Decalaration of a modelling subprogram using the ASVAT annotation.
   --  It is a procedure with no body (it has the aspect mark Import => True)
   --  All of its outputs, in this case all Globals of mode In_Out, will be
   --  set as a nodeterministic value when this procedure is called.
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

   --  This model subprogram has the same effect as Update_Nondet_Anno but
   --  is specified using pragma Import and as such cannot have a body.
   --  The global variables are specified using pragma Global.
   procedure Update_Nondet_Import;
   pragma Global (In_Out => (Visible_Vars.Var_Int,
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

   --  This model subprogram is the same as Update_Nondet_Anno except that
   --  it also marks each of the outputs as an "in type" value although the
   --  actual value is nondeterministic.
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

   --  The same effect as Update_Nondet_In_Type_Anno but specified using
   --  pragma Import and pragma Global.
   procedure Update_Nondet_In_Type_Import;
   pragma Global (In_Out => (Visible_Vars.Var_Int,
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

   procedure Nondet_Params_Anno (A : in     Integer;
                                 B : in out Integer;
                                 C : out    Integer)
     with Global   => (Output => Visible_Vars.Var_My_Sub_Enum),
          Annotate => (ASVAT, Nondet),
          Import   => True;

   procedure Nondet_Params_Import (A : in     Integer;
                                   B : in out Integer;
                                   C : out    Integer);
   pragma Global (Output => Visible_Vars.Var_My_Sub_Enum);
   pragma Import (Ada, Nondet_Params_Import, "Nondet");

   function Nondet_Integer_Anno return Visible_Types.My_Sub_Int
     with Global   => null,
          Annotate => (ASVAT, Nondet_In_Type),
          Import   => True;

   function Nondet_Integer_Import return Visible_Types.My_Sub_Int;
   pragma Global (null);
   pragma Import (Ada, Nondet_Integer_Import, "Nondet_In_Type");

   function Nondet_Integer_With_Body (X : Visible_Types.My_Sub_Int)
                                      return Visible_Types.My_Sub_Int is (X + 1)
     with Global   => null,
          Annotate => (ASVAT, Nondet_In_Type);

end The_Model;
