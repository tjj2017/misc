with Types, Snames;
--  # acquire Namet, Atree, Nlists, Sinfo;
package SPARK_Classic is
   --  SPARK annotations are translated in SPARK_Classic aspects or pragmas.
   --  Type Contexts is an enumeration which lists all the SPARK Classic
   --  aspects and pragmas.  Many of the aspects and pragmas create a context
   --  where other SPARK Classic aspects and paragmas are not allowed or
   --  required in a particular order.
   --  Blank_anno is used to represent any other non SPARK_Classic
   --  aspect or pragma.
   --  Additionally in SPARK Classic there are restictions the Ada context
   --  in which entities and pragmas may occur, so the type Contexts lists the
   --  relavent Ada contexts as well.

   type Contexts is
     (Start_Of_Annotations,
      --  SPARK annotations that apply to an Ada Entity or a SPARK declaration
      Classic_Acquire,
      Classic_Declare,
      Classic_Derives,
      Classic_Global,
      Classic_Initializes,
      Classic_Main_Program,
      Classic_Own,
      Classic_Own_Refinement,
      Classic_Pre,
      Classic_Post,
      Classic_Return,

      Start_Of_SPARK_Declarations,
      --  SPARK declarfations
      Classic_Function,
      Classic_Type,

      Start_Of_SPARK_Statements,
      --  SPARK_Statements
      Classic_Assert,
      Classic_Assume,
      Classic_Check,
      Classic_Hold,
      --  SPARK_Directives
      Classic_Hide,
      Classic_Blank,

      Start_Of_Ada_Contexts,
      --  Declaration contexts
      Generic_Package_Declaration,
      Generic_Procedure_Declaration,
      Generic_Function_Declaration,
      Package_Declaration,
      Package_Specification,
      Procedure_Declaration,
      Function_Declaration,
      Procedure_Specification,
      Function_Specification,
      Package_Body,
      Procedure_Body,
      Function_Body,

      Start_Of_Ada_Declarative_Parts,
      --  Declarative parts
      Package_Visible_Part,
      Package_Private_Part,
      Package_Body_Declarative_Part,
      Procedure_Declarative_Part,
      Function_Declarative_Part,
      Block_Declarative_Part,

      Start_Of_Ada_Statements,
      --  Sequences of statements
      Procedure_Statements,
      Function_Statements,
      Block_Statements,
      Package_Statements,
      Blank_Context,
      End_Of_Ada_Contexts);

   subtype SPARK_Classic_Annotations is Contexts range
     Contexts'Succ (Start_Of_Annotations) ..
     Contexts'Pred (Start_Of_Ada_Contexts);
   subtype Entity_Annotations is SPARK_Classic_Annotations range
     SPARK_Classic_Annotations'First ..
       Contexts'Pred (Start_Of_SPARK_Declarations);
   subtype SPARK_Declarations is SPARK_Classic_Annotations range
     Contexts'Succ (Start_Of_SPARK_Declarations) ..
     Contexts'Pred (Start_Of_SPARK_Statements);

   subtype Ada_Contexts is Contexts range
     Contexts'Succ (Start_Of_Ada_Contexts) ..
     Contexts'Pred (End_Of_Ada_Contexts);
   subtype Ada_Entity_Decs is Ada_Contexts range
     Contexts'Succ (Start_Of_Ada_Contexts) ..
     Contexts'Pred (Start_Of_Ada_Declarative_Parts);

   subtype Subprogram_Declaration is Ada_Contexts range
     Procedure_Declaration .. Function_Declaration;
   subtype Subprogram_Body is Ada_Contexts range
     Procedure_Body .. Function_Body;
   subtype Subprogram_Specification is Ada_Contexts range
     Procedure_Specification .. Function_Specification;
   subtype Generic_Subprogram_Declaration is Ada_Contexts range
     Generic_Procedure_Declaration .. Generic_Function_Declaration;
   subtype Subprogram_Declarative_Part is Ada_Contexts range
     Procedure_Declarative_Part .. Function_Declarative_Part;

   type Name_To_Annotation_Map is
     array (Snames.Classic_Annotation) of SPARK_Classic_Annotations;

   Name_To_Annotation : constant Name_To_Annotation_Map :=
     Name_To_Annotation_Map'
       (Snames.Name_Classic_Acquire => Classic_Acquire,
        Snames.Name_Classic_Assert => Classic_Assert,
        Snames.Name_Classic_Assume => Classic_Assume,
        Snames.Name_Classic_Check  => Classic_Check,
        Snames.Name_Classic_Declare => Classic_Declare,
        Snames.Name_Classic_Derives => Classic_Derives,
        Snames.Name_Classic_Function => Classic_Function,
        Snames.Name_Classic_Global => Classic_Global,
        Snames.Name_Classic_Hide => Classic_Hide,
        Snames.Name_Classic_Hold => Classic_Hold,
        Snames.Name_Classic_Initializes => Classic_Initializes,
        Snames.Name_Classic_Main_Program => Classic_Main_Program,
        Snames.Name_Classic_Own => Classic_Own,
        Snames.Name_Classic_Own_Refinement => Classic_Own_Refinement,
        Snames.Name_Classic_Pre => Classic_Pre,
        Snames.Name_Classic_Post => Classic_Post,
        Snames.Name_Classic_Return => Classic_Return,
        Snames.Name_Classic_Type => Classic_Type
       );

   function Context_Of_Node (N : Types.Node_Id) return Contexts;
   function Context_Of_Parent (N : Types.Node_Id) return Ada_Contexts;
   function Context_From_Declaration (Dec : Types.Node_Id)
                                      return Ada_Contexts;

end SPARK_Classic;
