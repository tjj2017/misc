with SPARK_Classic.Pragmas;
private package SPARK_Classic.Pragmas.Context_Defns is

   --  The following type is used to determine whether a Classic aspect or
   --  pragma is applied in an appropriate context and whether it placed in a
   --  suitable position, for instance, Classic_Own can only appear in a
   --  package declaration and must precede any Classic_Initializes.
   --  The meanings of the enumeration literals is:
   --  Only_This - only this context/placement is acceptable and no other
   --              Classic aspects/pragmas can precede it,
   --  This_Must -- this context/placement must exist but other Classic
   --               aspects/pragmas may precede it,
   --  Only_One  -- Given a number of contexts or placements any one but only
   --               one of them is acceptable,
   --  This      -- This context or placement is acceptable,
   --  Not_This  -- This context or placement is unacceptable,
   --  Nothing   -- No restriction present, mainly used as a sentinel.

   type Permissions is
     (Only_This, This_Must, Only_One, This, Not_This, Nothing, Not_Allowed);

   type Restriction is
      record
         Perm : Permissions;
         Ctx  : SPARK_Classic.Pragmas.Ada_Contexts;
      end record;

   Null_Restriction : constant Restriction :=
     Restriction'
       (Perm => Nothing,
        Ctx  => SPARK_Classic.Pragmas.Blank_Context);

   subtype Index is Positive range 1 .. 6;

   type Context_List is array (Index) of Restriction;

   Null_Context_List : constant Context_List := Context_List'
     (others => Null_Restriction);

   --  An Classic_Own aspect or pragma can only occur in the context
   --  of a package declaration.
   Own_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => Only_This,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      others => Null_Restriction);

   --  An Classic_Initializes aspect or pragma can only occur in the context
   --  of a package declaration.
   Initializes_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => Only_This,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      others => Null_Restriction);

   --  An Classic_Own_Refinement aspect or pragma can only occur in the context
   --  of a package body.
   Own_Refinement_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => Only_This,
         Ctx  => SPARK_Classic.Pragmas.Package_Body),
      others => Null_Restriction);

   --  A Classic Global, derives, pre or post aspect or pragma may occur within
   --  a subprogram declaration or subprogram body.
   --  In SPARK 2005 and SPARK Classic a subprogram declaration may only be
   --  placed in a package declaraion including the private part and the
   --  enclosing context is a package declaration.
   Global_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => Only_One,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      2 => Restriction'
        (Perm => Only_One,
         Ctx  => SPARK_Classic.Pragmas.Procedure_Body),
      3 => Restriction'
        (Perm => Only_One,
         Ctx  => SPARK_Classic.Pragmas.Function_Body),
      others => Null_Restriction);

   --  A SPARK Classic derives aspect or pragma an only occur in a procedure
   --  declaration or a procedure body.  The context of a procedure declaration
   --  will be a package declaration.  Ensuring that the derives is applied
   --  to a procedure declaration will have to done by placement restriction.
   Derives_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      2 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Procedure_Body),
      others => Null_Restriction);

   --  A SPARK Classic Pre
   Pre_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      2 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Procedure_Body),
      3 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Function_Body),
      others => Null_Restriction);

   --  A SPARK Classic post aspect or pragma can only occur in a procedure
   --  declaration or a procedure body.  The context of a procedure declaration
   --  will be a package declaration.  Ensuring that the derives is applied
   --  to a procedure declaration will have to done by placement restriction.
   Post_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      2 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Procedure_Body),
      others => Null_Restriction);

   --  A SPARK Classic return aspect or pragma can only occur in a procedure
   --  declaration or a procedure body.  The context of a procedure declaration
   --  will be a package declaration.  Ensuring that the derives is applied
   --  to a function declaration will have to done by placement restriction.
   Return_Context : constant Context_List := Context_List'
     (1 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Package_Declaration),
      2 => Restriction'
        (Perm => SPARK_Classic.Pragmas.Only_One,
         Ctx  => SPARK_Classic.Pragmas.Function_Body),
      others => Null_Restriction);

   type Restriction_Array is
     array (SPARK_Classic.Pragmas.SPARK_Classic_Annotations) of Context_List;

   Rules : constant Restriction_Array := Restriction_Array'
     (SPARK_Classic.Pragmas.Classic_Own             => Own_Context,
      SPARK_Classic.Pragmas.Classic_Initializes     => Initializes_Context,
      SPARK_Classic.Pragmas.Classic_Own_Refinement  => Own_Refinement_Context,
      SPARK_Classic.Pragmas.Classic_Global          => Global_Context,
      SPARK_Classic.Pragmas.Classic_Derives         => Derives_Context,
      SPARK_Classic.Pragmas.Classic_Pre             => Pre_Context,
      SPARK_Classic.Pragmas.Classic_Post            => Post_Context,
      SPARK_Classic.Pragmas.Classic_Return          => Return_Context,
      others => Null_Context_List);

   procedure Check_Context
     (N            : Types.Node_Id;
      Context      : out SPARK_Classic.Pragmas.Contexts;
      Pragma_Error : out SPARK_Classic.Pragmas.Pragma_Errors)
     with Pre => Atree.Nkind (N) = Sinfo.N_Pragma and then
                 Sinfo.Pragma_Name (N) in Snames.Classic_Annotation;

end SPARK_Classic.Pragmas.Context_Defns;
