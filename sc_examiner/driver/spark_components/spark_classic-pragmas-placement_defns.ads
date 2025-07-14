with SPARK_Classic.Pragmas;
--  # acquire SPARK_Classic, Types, Atree, Sinfo, Einfo, Sem_Util;
private package SPARK_Classic.Pragmas.Placement_Defns is

   --  SPARK annotations are translated into either SPARK_Classic
   --  aspect definitions or SPARK_Classic pragmas depending on the
   --  annotation and where it occurs.
   --  For each aspect definition the front-end introduces an equivalent
   --  pragma into the tree.
   --  Hence, the SPARK_Classic analyser can just process SPARK_Classic
   --  pragmas rather than aspect definitions.
   --  However, when looking at the position of a pragma which originates
   --  from an aspect it is easier to check through previous aspect definitions
   --  if the pragma derives from an aspect.
   --  The context and position of SPARK annotations is validated by
   --  checking the context in which the corresponding SPARK_Classic pragma
   --  is placed (its owner context) and its position within this context.
   --  The position is checked from the SPARK_Classic pragma currently being
   --  analysed back through previous pragmas (or aspect definitions) until
   --  a context is reached that is required (the validating context)
   --  or not allowed.
   --  A SPARK_Classic pragma has a Placement_List which specifies
   --  for each SPARK_Classic pragma (or aspect definition) preceding
   --  the current SPARK_Classic pragma whether it is required permitted or
   --  disallowed or is unrestricted.
   --  The following type is used to determine whether a Classic aspect or
   --  pragma is applied in an appropriate context and whether it placed in a
   --  suitable position, for instance, Classic_Own can only appear in a
   --  package declaration and must precede any Classic_Initializes.
   --  The meanings of the enumeration literals for a current pragma is:
   --  This_Must -- this context must precede the current SPARK Classic
   --               pragma but selected other aspects/pragmas may be placed
   --               between the current aspect/pragma and the required context
   --               by This_May placements,
   --  Only_One  -- Given a number of contexts any one but only one of them
   --               is required.  There may be intervening aspects/pragmas,
   --               placed between the current pragma and the required context,
   --  Suffices  -- encountering this context is sufficient to validate the
   --            -- position of the aspect/pragma being checked.
   --            -- For instance if the placement of a Classic_Pre aspect/
   --            -- pragma is being checked, encountering a Classic_Global
   --            -- aspect/pragma is sufficient to ensure the Classic_Pre
   --            -- is applied to a subprogram specification or body as
   --            -- the placement of the Classic_Global will be or has been
   --            -- checked when processed,
   --  This_May  -- This aspect/pragma may be placed between the current
   --               pragma and any required context,
   --  Not_Allowed -- This aspect/pragma or context is not allowed.
   --  Anything    -- No restriction present.

   type Permissions is
     (This_Must, Only_One, Suffices, This_May, Not_Allowed, Anything);

   --  A Placement_List is a permission list indexed by contexts.
   type Placement_List is array (SPARK_Classic.Contexts) of Permissions;

   --  Disallowed_Placement is a constant that represents Not_Allowed
   --  in any context.
   Disallowed_Placement : constant Placement_List := Placement_List'
     (others => Not_Allowed);

   --  An "owner" context is the context in which the aspect/pragma occurs.
   --  An owner context will always be an an Ada_Context.
   --  The type Owner_Contexts is a list of placements indexed by
   --  by owner context.
   type Owner_Contexts is array (SPARK_Classic.Ada_Contexts) of
     Placement_List;

   --  Disallowed_Context is a constant that represents not allowed in any
   --  context.
   Disallowed_Context : constant Owner_Contexts := Owner_Contexts'
     (others => Disallowed_Placement);

   --  The following constants represent the owners and placements allowed for
   --  each Classic pragma/aspect (annotations) identified by their name.

   --  A Classic_Own aspect or pragma can only occur in the owner context
   --  of a package visible part of a package specification for
   --  both a package declaration and a generic package declaration.
   --  It may only be placed as the first Classic aspect or pragma of
   --  a package visible part and cannot be preceded by any other
   --  Classic aspect definition or pragma.
   --  The validating context will be a package_specification.
   Own_Context : constant Owner_Contexts := Owner_Contexts'
     (SPARK_Classic.Package_Visible_Part => Placement_List'
        (SPARK_Classic.Package_Specification => This_Must,
         others => Not_Allowed),
      others => Disallowed_Placement);

   --  A Classic_Initializes aspect or pragma can only occur in the owner
   --  context of a package visible part of a package specification for both
   --  a package declaration and a generic package declaration.
   --  It may only be placed immediately following an Classic_Own aspect
   --  or pragma and cannot be preceded by any other Classic aspect or pragma.
   --  The validating context will be a Classic_Own context.
   Initializes_Context : constant Owner_Contexts := Owner_Contexts'
       (SPARK_Classic.Package_Visible_Part => Placement_List'
          (SPARK_Classic.Classic_Own => This_Must,
          others => Not_Allowed),
        others => Disallowed_Placement);

   --  A Classic_Own_Refinement aspect or pragma can only occur in the
   --  owner context of a package body declarative part.
   --  It may only be placed as the first and only Classic aspect or
   --  pragma of a package body declarative part.
   --  The validating context will be a package body.
   Own_Refinement_Context : constant Owner_Contexts := Owner_Contexts'
       (SPARK_Classic.Package_Body_Declarative_Part => Placement_List'
          (SPARK_Classic.Package_Body => This_Must,
           others => Not_Allowed),
        others => Disallowed_Placement);

   --  A Classic Global, derives, pre or post aspect or pragma may occur within
   --  a subprogram declaration or subprogram body.
   --  In SPARK 2005 and SPARK Classic a subprogram declaration may only be
   --  placed in a package declaration including the private part. Then
   --  owner context is a package visible or private part.
   --  If the aspect or pragma occurs in a subprogram body its
   --  owner context must be a subprogram declarative part.

   --  A Classic_Global aspect or pragma may only be placed as the first
   --  aspect or pragma of a subprogram declaration or subprogram body.
   --  If it is subprogram declaration its owner will be the visible or
   --  private part of a package specification.
   --  The Classic Global aspect or pragma must be the first Classic
   --  aspect or pragma following a procedure or function specification.
   --  If it is an aspect its validating context will be a
   --  subprogram_specification and for a pragma it will be a
   --  subprogram_declaration.
   --  If Classic_Global aspect or pragma is placed in a subprogram body its
   --  owner context will be a subprogram declarative part.
   --  It must be the first Classic aspect
   --  or pragma of the declarative part of the subprogram.
   --  For an aspect the validating context, when no preceding aspects are
   --  present in the subprogram body, will be a subprogram_specification.
   --  For a pragma the validating context a subprogram body.
   Global_Context : constant Owner_Contexts := Owner_Contexts'
     (SPARK_Classic.Package_Visible_Part |
      SPARK_Classic.Package_Private_Part => Placement_List'
        (SPARK_Classic.Subprogram_Declaration |
         SPARK_Classic.Subprogram_Specification => Only_One,
         others => Not_Allowed),
      SPARK_Classic.Subprogram_Declarative_Part => Placement_List'
        (SPARK_Classic.Subprogram_Specification |
        SPARK_Classic.Subprogram_Body => Only_One,
         others => Not_Allowed),
      others => Disallowed_Placement);

   --  A SPARK Classic derives aspect or pragma may only occur in a procedure
   --  declaration or a procedure body.  It may be the first aspect or pragma
   --  of the procedure declaration or body, or it may follow a Classic_Global
   --  aspect or pragma.
   --  If it is a associated with a procedure declaration, then its owner
   --  context will be the visible or private part of a package and it must
   --  follow a procedure specification possibly with an intervining
   --  Classic Global aspect/pragma.  The validating context will be a
   --  procedure specification for an aspect or a procedure declaration for
   --  a pragma.
   --  If it is in a procedure body its owner context will be a
   --  procedure_declarative part and it must be the first Classic aspect or
   --  pragma of the declarative part, possibly with an intervining
   --  Classic global.  If it is an aspect,  when no preceding aspects are
   --  present in the subprogram body, the validating context will be a
   --  procedure specification,
   --  if is a pragma its validating context will be a procedure body.
   Derives_Context : constant Owner_Contexts := Owner_Contexts'
     (SPARK_Classic.Package_Visible_Part |
      SPARK_Classic.Package_Private_Part => Placement_List'
        (SPARK_Classic.Classic_Global => This_May,
         SPARK_Classic.Procedure_Declaration |
         SPARK_Classic.Procedure_Specification => Only_One,
         others => Not_Allowed),
      SPARK_Classic.Procedure_Declarative_Part => Placement_List'
           (SPARK_Classic.Classic_Global => This_May,
            SPARK_Classic.Procedure_Specification |
            SPARK_Classic.Procedure_Body => Only_One,
            others => Not_Allowed),
       others => Disallowed_Placement);

   --  A SPARK Classic_Pre may be placed as the first aspect or pragma in
   --  a subprogram declaration or body or it may immediately follow a
   --  Classic_Global or Classic_Derives aspect or pragma.
   --  If it is placed after a subprogram declaration its owner context will
   --  be the visible or private part of a package.
   --  If it is placed in a subprogram body its owner context will be a
   --  subprogram declarative part.  It may follow a derives or global
   --  aspect/pragma but must follow a subprogram specification.
   --  The validating context of a Classic_Pre aspect, when no preceding
   --  aspects are present in the subprogram body, will still be a
   --  subprogram specification but for a pragma the validating context
   --  will be a subprogram body.
   Pre_Context : constant Owner_Contexts := Owner_Contexts'
     (SPARK_Classic.Package_Visible_Part |
      SPARK_Classic.Package_Private_Part => Placement_List'
        (SPARK_Classic.Classic_Derives |
         SPARK_Classic.Classic_Global => Suffices,
         SPARK_Classic.Subprogram_Declaration |
         SPARK_Classic.Subprogram_Specification => Only_One,
         others => Not_Allowed),
      SPARK_Classic.Subprogram_Declarative_Part => Placement_List'
        (SPARK_Classic.Classic_Derives |
         SPARK_Classic.Classic_Global => Suffices,
         SPARK_Classic.Subprogram_Specification |
         SPARK_Classic.Subprogram_Body => Only_One,
         others => Not_Allowed),
      others => Disallowed_Placement);

   --  A SPARK Classic Post aspect or pragma may only occur in a procedure
   --  declaration or a procedure body.  It may be the first aspect or pragma
   --  of the procedure declaration or body, or it may follow a Classic_Global,
   --  Derives or Pre aspect or pragma.
   --  If it is a associated with a procedure declaration, then its owner
   --  context will be the visible or private part of a package.
   --  If it is associated with a procedure body its owner context will be
   --  a procedure declarative part.
   --  In both a declaration and body it must follow a procedure specification
   --  possibly with an intervining Classic Global Derives or Pre.
   --  If the Classic_Post is an aspect, with no preceding aspects
   --  present in the subprogram body, the validating context will be a
   --  procedure specification, but for a pragma its validating_context
   --  will be a procedure body.
   Post_Context : constant Owner_Contexts := Owner_Contexts'
     (SPARK_Classic.Package_Visible_Part |
      SPARK_Classic.Package_Private_Part => Placement_List'
        (SPARK_Classic.Classic_Pre |
         SPARK_Classic.Classic_Global => This_May,
         SPARK_Classic.Classic_Derives => Suffices,
         SPARK_Classic.Procedure_Declaration |
         SPARK_Classic.Procedure_Specification => Only_One,
         others => Not_Allowed),
      SPARK_Classic.Procedure_Declarative_Part => Placement_List'
        (SPARK_Classic.Classic_Pre |
         SPARK_Classic.Classic_Global => This_May,
         SPARK_Classic.Classic_Derives => Suffices,
         SPARK_Classic.Procedure_Specification |
         SPARK_Classic.Procedure_Body => Only_One,
         others => Not_Allowed),
      others => Disallowed_Placement);

   --  A SPARK Classic Return aspect or pragma may only occur in a function
   --  declaration or a function body.  It may be the first aspect or pragma
   --  of the function declaration or body, or it may follow a Classic_Global,
   --  or Pre aspect or pragma.
   --  If it is a associated with a function declaration, then its owner
   --  context will be the visible or private part of a package.
   --  If it is associated with a function body then its owner context will be
   --  a function declarative part.
   --  For a Classic_Return aspect with no intervining global aspect,
   --  the validating context of the aspect will be a function specification
   --  and for a pragma associated with a funtion declaration it will be a
   --  function declaration and when associated with a function body its
   --  validating context will be a funtion body.
   Return_Context : constant Owner_Contexts := Owner_Contexts'
     (SPARK_Classic.Package_Visible_Part |
      SPARK_Classic.Package_Private_Part => Placement_List'
        (SPARK_Classic.Classic_Pre |
         SPARK_Classic.Classic_Global => This_May,
         SPARK_Classic.Function_Declaration |
         SPARK_Classic.Function_Specification => Only_One,
         others => Not_Allowed),
      SPARK_Classic.Function_Declarative_Part => Placement_List'
        (SPARK_Classic.Classic_Pre |
         SPARK_Classic.Classic_Global => This_May,
         SPARK_Classic.Function_Specification |
         SPARK_Classic.Function_Body => Only_One,
         others => Not_Allowed),
      others => Disallowed_Placement);

   --  A placement array is a list of owner contexts indexed by
   --  SPARK Classic annotations.
   type Placement_Array is
     array (SPARK_Classic.SPARK_Classic_Annotations) of Owner_Contexts;

   --  The constant Rules is indexed by SPARK annotations, then
   --  further indexed by owner context to provide a list of permissions
   --  which is indexed by Contexts.
   --  In other words the Rules array is gives a specification
   --  of the allowed owner context and placement for each
   --  SPARK Classic annotation.
   --  For clarity each component of the array is defined by the individual
   --  <annotation>_Context constants declared above.
   Rules : constant Placement_Array := Placement_Array'
     (SPARK_Classic.Classic_Own            => Own_Context,
      SPARK_Classic.Classic_Initializes    => Initializes_Context,
      SPARK_Classic.Classic_Own_Refinement => Own_Refinement_Context,
      SPARK_Classic.Classic_Global         => Global_Context,
      SPARK_Classic.Classic_Derives        => Derives_Context,
      SPARK_Classic.Classic_Pre            => Pre_Context,
      SPARK_Classic.Classic_Post           => Post_Context,
      SPARK_Classic.Classic_Return         => Return_Context,
      others => Disallowed_Context);

   --  Check_Placement checks the context and placement of a SPARK Classic
   --  annotation according to the rules specified above.
   --  It outputs the atree node to which the aspect/pragma applies and its
   --  Ada_Context.
   --  If the annotation is a SPARK_Specification it also outputs the entity
   --  to which the annotation applies, otherwise the parameter Applies_Entity
   --  is Empty.
   --  If an error is encountered the Applies_To, Applies_Context and
   --  Applies_Entity may be empty.
   procedure Check_Placement
     (N              : Types.Node_Id;
      Classic_Anno   : SPARK_Classic.SPARK_Classic_Annotations;
      Parent_Context : SPARK_Classic.Ada_Contexts;
      Applies_To     : out Types.Node_Id;
      Applies_Ctx    : out SPARK_Classic.Ada_Contexts;
      Applies_Entity : out Types.Entity_Id;
      Pragma_Error   : out SPARK_Classic.Pragmas.Pragma_Errors)
     with
       Pre => Atree.Nkind (N) = Sinfo.N_Pragma and then
              Sinfo.Pragma_Name (N) in Snames.Classic_Annotation;

end SPARK_Classic.Pragmas.Placement_Defns;
