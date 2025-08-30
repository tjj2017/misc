with Text_IO;
pragma Warnings (Off, Text_IO);
with Snames,
     Types,
     Sinfo,
     Atree;
use type Sinfo.Node_Kind;
--  # acquire SPARK_Classic,
--  # Types, Namet, Nlists, Elists, Sem_Util, Einfo, Sem_Eval, Uintp;
package SPARK_Classic.Pragmas is
   type Modifiers is
     (Plain,
      External_In,
      External_Out,
      Own_Task,
      Protected_Own,
      Protected_In,
      Protected_Out,
      Illegal_Modifier);

   type Properties is
     (Own_Type,
      Integrity,
      Priority,
      Interrupt,
      Protects,
      Suspendable,
      May_Delay,
      Suspends,
      Illegal_Property);

   type Mod_Props is
     (Input,
      Output,
      In_Out,
      Plain,
      External_In,
      External_Out,
      Own_Task,
      Protected_Own,
      Protected_In,
      Protected_Out,

      Start_Of_Properties,

      Own_Type,
      Integrity,
      Priority,
      Interrupt,
      Protects,
      Suspendable,
      May_Delay,
      Suspends,
      Illegal_Mod_Prop);

   subtype Mods is Mod_Props range
     Mod_Props'First .. Mod_Props'Pred (Start_Of_Properties);
   subtype Props is Mod_Props range
     Mod_Props'Succ (Start_Of_Properties) ..
       Mod_Props'Pred (Illegal_Mod_Prop);
   subtype Global_Mods is Mods range Mods'First .. Plain;
   subtype Own_Mods is Mods range Plain .. Mods'Last;
   subtype Own_Props is Props range Own_Type .. Suspendable;
   subtype Declare_Props is Props range May_Delay .. Suspends;

   type Mod_Prop_Node_Pair is
      record
         Mod_Or_Prop : Mod_Props;
         Node        : Types. Node_Id;
      end record;

   subtype Own_Item_Properties is Properties range Own_Type .. Suspendable;
   subtype Name_Only_Properties is Properties range Suspendable ..
     May_Delay;

   subtype Own_Modifiers is Modifiers range Plain .. Protected_Out;

   procedure Process (N : Types.Node_Id)
     with Pre => Atree.Nkind (N) = Sinfo.N_Pragma;

private

   type Name_To_Anno_Map is array (Snames.Classic_Annotation) of
     SPARK_Classic_Annotations;

   Name_To_Anno : constant Name_To_Anno_Map := Name_To_Anno_Map'
     (Snames.Name_Classic_Acquire         => SPARK_Classic.Classic_Acquire,
      Snames.Name_Classic_Assert         => SPARK_Classic.Classic_Assert,
      Snames.Name_Classic_Assume         => SPARK_Classic.Classic_Assume,
      Snames.Name_Classic_Check          => SPARK_Classic.Classic_Check,
      Snames.Name_Classic_Declare        => SPARK_Classic.Classic_Declare,
      Snames.Name_Classic_Derives        => SPARK_Classic.Classic_Derives,
      Snames.Name_Classic_Function       => SPARK_Classic.Classic_Function,
      Snames.Name_Classic_Global         => SPARK_Classic.Classic_Global,
      Snames.Name_Classic_Hide           => SPARK_Classic.Classic_Hide,
      Snames.Name_Classic_Hold           => SPARK_Classic.Classic_Hold,
      Snames.Name_Classic_Initializes    => SPARK_Classic.Classic_Initializes,
      Snames.Name_Classic_Main_Program   => SPARK_Classic.Classic_Main_Program,
      Snames.Name_Classic_Own            => SPARK_Classic.Classic_Own,
      Snames.Name_Classic_Own_Refinement =>
        SPARK_Classic.Classic_Own_Refinement,
      Snames.Name_Classic_Pre            => SPARK_Classic.Classic_Pre,
      Snames.Name_Classic_Post           => SPARK_Classic.Classic_Post,
      Snames.Name_Classic_Return         => SPARK_Classic.Classic_Return,
      Snames.Name_Classic_Type           => SPARK_Classic.Classic_Type);

   type Pragma_Errors is
     (None,
      Wrong_Only_Context,
      Invalid_Context,
      Unsuitable_Context,
      Not_1_Argument,
      Named_Argument,
      Own_Not_In_Package_Dec,
      Misplaced_Annotation,
      More_Than_One_Context,
      Not_An_Aggregate,
      Not_A_Comp_Assoc_Or_Extension,
      Null_Extension,
      Wrong_Aggregate,
      Not_List_Of_Aggregates,
      Wrong_Ancestor,
      Expr_List_Not_Allowed,
      Not_A_Nameless_Comp_Assoc,
      Not_Single_Argument,
      Not_1_Choice,
      Unknown_Modifier,
      Unsuitable_Modifier,
      Unknown_Property,
      Unsuitable_Property,
      Multi_Property_Choices,
      Invalid_Own_Modifier,
      Invalid_Own_Item,
      Invalid_Own_Property,
      Duplicate_Own_Item,
      Own_Item_Insertion,
      Name_Only_Property_Not_True,
      Illegal_Interrupt_Source,
      Wrong_Property_Aggregate,
      Mixed_Positional_And_Named,
      Ancestor_Part_Is_Extension,
      Properties_Not_Allowed,
      Not_Static_Integer_Expr,
      Not_An_Expanded_Name,
      Integrity_Not_Natural,
      Own_Type_Not_A_Name,
      Duplicate_Properties,
      Illegal_Property,
      Illegal_Redclaration,
      Name_Is_Not_A_Type_Mark,
      Own_Type_Not_Applied_To_Own_Var
     );

   function Has_Chars (N : Types.Node_Id; S : String) return Boolean
   with Pre => Atree.Nkind (N) = Sinfo.N_Identifier;

end SPARK_Classic.Pragmas;
