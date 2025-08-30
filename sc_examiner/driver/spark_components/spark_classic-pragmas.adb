with Text_IO; use Text_IO;
with Types, Namet, Nlists, Sem_Util, Einfo, Sem_Eval, Uintp;
use type Namet.Name_Id, Einfo.Entity_Kind, Types.Int;
with Treepr; use Treepr;
with SPARK_Classic.Pragmas.Placement_Defns,
     SPARK_Classic.Pragmas.Atree_Utils;
--     SPARK_Classic.Symbols;
package body SPARK_Classic.Pragmas is

   type Aggregate_Sort is (Comp_Assocs, Exprs, Mixed, Extension, Unknown);

   type Arg_Sorts is
     (Expr_List,
      Comp_Assocs,
      Single_Extension_Comp_Assocs,
      Comp_Assocs_Extension_Comp_Assocs,
      Comp_Assocs_Extension_Expr_List,
      Single_Extension_Expr_List,
      Expr_List_Extension_Expr_List,
      Expr_List_Extension_Comp_Assocs,
      Not_Aggregate,
      Mixed_Pos_And_Named,
      Ancestor_Extension
     );

   subtype Own_Variable_Arg_Sorts is Arg_Sorts range
     Arg_Sorts'First .. Comp_Assocs_Extension_Comp_Assocs;
--     subtype Own_Var_Refinment_Arg_Sorts is Arg_Sorts range
--       Single_Extension_Expr_List .. Single_Extension_Expr_List;
--     subtype Global_Arg_Sorts is Arg_Sorts range
--       Comp_Assocs .. Comp_Assocs;
--     subtype Derives_Arg_Sorts is Arg_Sorts range
--       Single_Extension_Null .. Null_Extesion_Null;
--     subtype Declare_Arg_Sorts is Arg_Sorts range
--       Comp_Assocs .. Comp_Assocs;

   function Has_Chars (N : Types.Node_Id; S : String) return Boolean
   --  --# pre Atree.NKind (N) = Sinfo.N_Identifier;
   is
      --  --# hide Has_Chars;
   begin
      return Namet.Get_Name_String (Sinfo.Chars (N)) = S;
   end Has_Chars;

   function Chars_Length (N : Types.Node_Id) return Natural
   with Pre => Atree.Nkind (N) = Sinfo.N_Identifier;

   function Chars_Length (N : Types.Node_Id) return Natural
   --  --# pre Atree.NKind (N) = Sinfo.N_Identifier;
   is
      --  --# hide Chars_Length;
   begin
      return Natural (Namet.Length_Of_Name (Sinfo.Chars (N)));
   end Chars_Length;

   function Mod_Prop_From_Node (N : Types.Node_Id) return Mod_Props
     with Pre => Atree.Nkind (N) = Sinfo.N_Identifier;

   function Mod_Prop_From_Node (N : Types.Node_Id) return Mod_Props
   is
      Str_Len : constant Natural := Chars_Length (N);
      Result  : Mod_Props;
   begin
      Result := Illegal_Mod_Prop;
      case Str_Len is
         when 5 =>
            if Has_Chars (N, "input") then
               Result := Input;
            elsif Has_Chars (N, "plain") then
               Result := Plain;
            end if;
         when 6 =>
            if Has_Chars (N, "output") then
               Result := Output;
            elsif Has_Chars (N, "in_out") then
               Result := In_Out;
            end if;
         when 8 =>
            if Has_Chars (N, "own_task") then
               Result := Own_Task;
            elsif Has_Chars (N, "own_type") then
               Result := Own_Type;
            elsif Has_Chars (N, "priority") then
               Result := Priority;
            elsif Has_Chars (N, "protects") then
               Result := Protects;
            elsif Has_Chars (N, "suspends") then
               Result := Suspends;
            end if;
         when 9 =>
            if Has_Chars (N, "integrity") then
               Result := Integrity;
            elsif Has_Chars (N, "interrupt") then
               Result := Interrupt;
            elsif Has_Chars (N, "may_delay") then
               Result := May_Delay;
            end if;
         when 11 =>
            if Has_Chars (N, "external_in") then
               Result := External_In;
            elsif Has_Chars (N, "suspendable") then
               Result := Suspendable;
            end if;
         when 12 =>
            if Has_Chars (N, "external_out") then
               Result := External_Out;
            elsif Has_Chars (N, "protected_in") then
               Result := Protected_In;
            end if;
         when 13 =>
            if Has_Chars (N, "protected_own") then
               Result := Protected_Own;
            elsif Has_Chars (N, "protected_out") then
               Result := Protected_Out;
            end if;
         when others =>
            --  Result remains Illegal_Mod_Prop.
            null;
      end case;
      return Result;
   end Mod_Prop_From_Node;

   function Modifier_From_Node (N : Types.Node_Id) return Modifiers
     with Pre => Atree.Nkind (N) = Sinfo.N_Identifier;

   function Modifier_From_Node (N : Types.Node_Id) return Modifiers
   --  --# pre => Sinfo.Nkind (N) = Sinfo.N_Identifier;
   is
      Res : Modifiers;
   begin
      if Has_Chars (N, "plain") then
         Res := Plain;
      elsif Has_Chars (N, "external_in") then
         Res := External_In;
      elsif Has_Chars (N, "external_out") then
         Res := External_Out;
      elsif Has_Chars (N, "own_task") then
         Res := Own_Task;
      elsif Has_Chars (N, "protected_own") then
         Res := Protected_Own;
      elsif Has_Chars (N, "protected_in") then
         Res := Protected_In;
      elsif Has_Chars (N, "protected_out") then
         Res := Protected_Out;
      else
         Res := Illegal_Modifier;
      end if;
      return Res;
   end Modifier_From_Node;

   function Property_From_Node (N : Types.Node_Id) return Properties
     with Pre => Atree.Nkind (N) = Sinfo.N_Identifier;

   function Property_From_Node (N : Types.Node_Id) return Properties
   --  --# pre => Sinfo.Nkind (N) = Sinfo.N_Identifier;
   is
      Res : Properties;
   begin
      if Has_Chars (N, "own_type") then
         Res := Own_Type;
      elsif Has_Chars (N, "integrity") then
         Res := Integrity;
      elsif Has_Chars (N, "priority") then
         Res := Priority;
      elsif Has_Chars (N, "suspendable") then
         Res := Suspendable;
      elsif Has_Chars (N, "interrupt") then
         Res := Interrupt;
      elsif Has_Chars (N, "protects") then
         Res := Protects;
      elsif Has_Chars (N, "suspends") then
         Res := Suspends;
      else
         Res := Illegal_Property;
      end if;
      return Res;
   end Property_From_Node;

   function Get_Aggregate_Sort (Aggregate : Types.Node_Id)
                                return Aggregate_Sort;

   function Get_Aggregate_Sort (Aggregate : Types.Node_Id)
                                return Aggregate_Sort is
      Agg_Kind : constant Sinfo.Node_Kind := Atree.Nkind (Aggregate);
      Result : Aggregate_Sort;
   begin
      if Agg_Kind = Sinfo.N_Aggregate then
         if Nlists.Present (Sinfo.Component_Associations (Aggregate)) and then
           not Nlists.Present (Sinfo.Expressions (Aggregate))
         then
            Result := Comp_Assocs;
         elsif Nlists.Present (Sinfo.Expressions (Aggregate)) and then
           not Nlists.Present (Sinfo.Component_Associations (Aggregate))
         then
            Result := Exprs;
         else
            Result := Mixed;
         end if;
      elsif Agg_Kind = Sinfo.N_Extension_Aggregate then
         Result := Extension;
      else
         Result := Unknown;
      end if;
      return Result;
   end Get_Aggregate_Sort;

   function Get_Arg_Sort (Argument : Types.Node_Id) return Arg_Sorts;

   function Get_Arg_Sort (Argument : Types.Node_Id) return Arg_Sorts is
      type Ancestor_Sorts is
        (Single_Ancestor,
         Comp_Assoc_Ancestor,
         Expr_List_Ancestor,
         Mixed_Ancestor,
         Extension_Ancestor
        );
      Result            : Arg_Sorts;
      Arg_Kind          : Sinfo.Node_Kind;
      Ancestor          : Types.Node_Id;
      Ancestor_Sort     : Ancestor_Sorts;
      Arg_Exprs         : Types.List_Id;
      Arg_Comp_Assocs   : Types.List_Id;
   begin
      Arg_Kind := Atree.Nkind (Argument);
      if Arg_Kind = Sinfo.N_Aggregate then
         Arg_Exprs := Sinfo.Expressions (Argument);
         Arg_Comp_Assocs := Sinfo.Component_Associations (Argument);
         if Nlists.Present (Arg_Exprs) and then
           not Nlists.Present (Arg_Comp_Assocs)
         then
            Result := Expr_List;
         elsif Nlists.Present (Arg_Comp_Assocs) and then
           not Nlists.Present (Arg_Exprs)
         then
            Result := Comp_Assocs;
         else
            Result := Mixed_Pos_And_Named;
         end if;

      elsif Arg_Kind = Sinfo.N_Extension_Aggregate then
         Ancestor := Sinfo.Ancestor_Part (Argument);
         Arg_Exprs := Sinfo.Expressions (Ancestor);
         Arg_Comp_Assocs := Sinfo.Component_Associations (Ancestor);
         case Atree.Nkind (Ancestor) is
            when Sinfo.N_Aggregate =>
               if Nlists.Present (Arg_Exprs) and
                 not Nlists.Present (Arg_Comp_Assocs)
               then
                  Ancestor_Sort := Expr_List_Ancestor;
               elsif Nlists.Present (Arg_Comp_Assocs)
                 and not Nlists.Present (Arg_Exprs)
               then
                  Ancestor_Sort := Comp_Assoc_Ancestor;
               else
                  Ancestor_Sort := Mixed_Ancestor;
               end if;

            when Sinfo.N_Extension_Aggregate =>
               Ancestor_Sort := Extension_Ancestor;
               Result := Ancestor_Extension;
            when others =>
               Ancestor_Sort := Single_Ancestor;
         end case;
         Arg_Exprs := Sinfo.Expressions (Argument);
         Arg_Comp_Assocs := Sinfo.Component_Associations (Argument);
         if Nlists.Present (Arg_Exprs) and then
           not Nlists.Present (Arg_Comp_Assocs)
         then
            case Ancestor_Sort is
               when Expr_List_Ancestor =>
                  Result := Expr_List_Extension_Expr_List;
               when Comp_Assoc_Ancestor =>
                  Result := Comp_Assocs_Extension_Expr_List;
               when Single_Ancestor =>
                  Result := Single_Extension_Expr_List;
               when Extension_Ancestor =>
                  Result := Ancestor_Extension;
               when Mixed_Ancestor =>
                  Result := Mixed_Pos_And_Named;
            end case;
         elsif Nlists.Present (Arg_Comp_Assocs)
           and then not Nlists.Present (Arg_Exprs)
         then
            case Ancestor_Sort is
               when Expr_List_Ancestor =>
                  Result := Expr_List_Extension_Comp_Assocs;
               when Comp_Assoc_Ancestor =>
                  Result := Comp_Assocs_Extension_Comp_Assocs;
               when Single_Ancestor =>
                  Result := Single_Extension_Comp_Assocs;
               when Extension_Ancestor =>
                  Result := Ancestor_Extension;
               when Mixed_Ancestor =>
                  Result := Mixed_Pos_And_Named;
            end case;
         end if;
      else
         Result := Not_Aggregate;
      end if;
      return Result;
   end Get_Arg_Sort;

   procedure Get_Pragma_Argument (N            : Types.Node_Id;
                                  Argument     : out Types.Node_Id;
                                  Arg_Sort     : out Arg_Sorts;
                                  Pragma_Error : out Pragma_Errors);

   procedure Get_Pragma_Argument (N            : Types.Node_Id;
                                  Argument     : out Types.Node_Id;
                                  Arg_Sort     : out Arg_Sorts;
                                  Pragma_Error : out Pragma_Errors) is

      Arg               : Types.Node_Id;
   begin
      --  Assume no syntax errors
      Pragma_Error := None;
      Argument := Types.Empty;
      Arg_Sort := Not_Aggregate;

      --  SPARK Classic pragma has only 1 argument
      if Nlists.Present (Sinfo.Pragma_Argument_Associations (N)) and then
        Nlists.List_Length (Sinfo.Pragma_Argument_Associations (N)) = 1
      then
         Arg :=
           Nlists.First (Sinfo.Pragma_Argument_Associations (N));
      else
         Pragma_Error := Not_1_Argument;
      end if;

      --  A SPARK Classic pragma should not have a named argument
      if Pragma_Error = None and then
        not (Atree.Nkind (Arg) = Sinfo.N_Pragma_Argument_Association and then
                      Sinfo.Chars (Arg) = Namet.No_Name)
      then
         Pragma_Error := Named_Argument;
      end if;

      Put_Line ("Get_Prag_Arg");
      if Pragma_Error = None then
         Argument := Sinfo.Expression (Arg);
         Arg_Sort := Get_Arg_Sort (Argument);
         if Arg_Sort = Mixed_Pos_And_Named then
            Pragma_Error := Mixed_Positional_And_Named;
         end if;
      end if;
   end Get_Pragma_Argument;

   procedure Get_Single_Assoc_Choice (Comp_Assoc   : Types.Node_Id;
                                      Choice       : out Types.Node_Id;
                                      Pragma_Error : out Pragma_Errors);

   procedure Get_Single_Assoc_Choice (Comp_Assoc   : Types.Node_Id;
                                      Choice       : out Types.Node_Id;
                                      Pragma_Error : out Pragma_Errors)
   is
      Ch_List : Types.List_Id;
   begin
      Pragma_Error := None;
      if Atree.Nkind (Comp_Assoc) = Sinfo.N_Component_Association then
         Ch_List := Sinfo.Choices (Comp_Assoc);
         if Nlists.List_Length (Ch_List) = 1 then
            Choice := Nlists.First (Ch_List);
         else
            Pragma_Error := Not_1_Choice;
         end if;
      else
         Pragma_Error := Not_A_Nameless_Comp_Assoc;
      end if;
   end Get_Single_Assoc_Choice;

   procedure Get_Modifier (Comp_Assoc   : Types.Node_Id;
                           Modifier     : out Modifiers;
                           Pragma_Error : out Pragma_Errors);

   procedure Get_Modifier (Comp_Assoc   : Types.Node_Id;
                           Modifier     : out Modifiers;
                           Pragma_Error : out Pragma_Errors) is
      Choice : Types.Node_Id;
   begin
      Get_Single_Assoc_Choice (Comp_Assoc, Choice, Pragma_Error);
      if Pragma_Error = None then
         Modifier := Modifier_From_Node (Choice);
         if Modifier not in Own_Modifiers then
            if Modifier = Illegal_Modifier then
               Pragma_Error := Unknown_Modifier;
            else
               Pragma_Error := Unsuitable_Modifier;
            end if;
         end if;
      else
         Modifier := Illegal_Modifier;
      end if;
   end Get_Modifier;

   procedure Get_Property (Comp_Assoc   : Types.Node_Id;
                           Property     : out Properties;
                           Pragma_Error : out Pragma_Errors);

   procedure Get_Property (Comp_Assoc   : Types.Node_Id;
                           Property     : out Properties;
                           Pragma_Error : out Pragma_Errors) is
      Choice : Types.Node_Id;
   begin
      Get_Single_Assoc_Choice (Comp_Assoc, Choice, Pragma_Error);
      if Pragma_Error = None then
         Property := Property_From_Node (Choice);
         if Property not in Own_Item_Properties then
            if Property =  Illegal_Property then
               Pragma_Error := Unknown_Property;
            else
               Pragma_Error := Unsuitable_Property;
            end if;
         end if;
      else
         Property := Illegal_Property;
      end if;
   end Get_Property;

   procedure Check_Own_Vars (Pairs          : Types.List_Id;
--                               Sym_With_Props : SPARK_Classic.Symbols.Symbol;
                             Pragma_Error   : out Pragma_Errors);
   --  # global SPARK_Classic.Symbols.Table;

   procedure Check_Own_Vars (Pairs          : Types.List_Id;
--                               Sym_With_Props : SPARK_Classic.Symbols.Symbol;
                             Pragma_Error : out Pragma_Errors)
   is
      Curr_Pair      : Types.Node_Id := Nlists.First (Pairs);
      Curr_Own_Var   : Types.Node_Id;
      Curr_Mod_Prop  : Mod_Props;
      Curr_Entity    : Types.Entity_Id;
--        Curr_Symbol    : SPARK_Classic.Symbols.Symbol;
--        Curr_Symbol_Id : SPARK_Classic.Symbols.Symbol_Id;
--        Curr_Skind     : SPARK_Classic.Symbols.Symbol_Kind;
      Enclosing_Declaration : constant Types.Node_Id :=
          Sem_Util.Enclosing_Declaration (Curr_Pair);
--        Inserted       : Boolean;
   begin
      Put_Line ("Check_Own_Vars");
      if Atree.Nkind (Enclosing_Declaration) = Sinfo.N_Package_Declaration
      then
         Pragma_Error := None;
      else
         --  An own variable can only be declared within a package declaration.
         --  Placement checks should mean that this branch is never entered.
         Pragma_Error := Own_Not_In_Package_Dec;
      end if;
      while Pragma_Error = None and Atree.Present (Curr_Pair) loop
         Put_Line ("Get a pair");
         if Nlists.List_Length (Sinfo.Choices (Curr_Pair)) = 1 then
            Curr_Mod_Prop :=
              Mod_Prop_From_Node (Nlists.First (Sinfo.Choices (Curr_Pair)));
            if Curr_Mod_Prop in Own_Mods then
               Curr_Own_Var := Sinfo.Expression (Curr_Pair);
               if Atree.Nkind (Curr_Own_Var) = Sinfo.N_Identifier then
                  --  The declared own variable must be an identifier.
                  Curr_Entity := SPARK_Classic.Pragmas.Atree_Utils.
                    Find_Entity_From_Name (Curr_Own_Var);
                  Put_Line ("Curr_Own_Var Entity");
                  Print_Node_Briefly (Curr_Entity);
                  Put_Line ("Modifier: " &
                              Own_Mods'Image (Curr_Mod_Prop));
                  Print_Node_Briefly (Curr_Own_Var);
                  if Atree.Present (Curr_Entity) and then
                    Atree.Ekind (Curr_Entity) /= Einfo.E_Void
                  then
                     Put_Line ("Has entity - illegal redeclaration of " &
                            "directly visible variable");
                     Pragma_Error := Illegal_Redclaration;
                  else
                     Put_Line ("Does not have Entity");
                     Print_Node_Briefly
                       (Sem_Util.Unique_Defining_Entity
                          (Enclosing_Declaration));
                     Put_Line
                       (Sem_Util.Unique_Name
                          (Sem_Util.Unique_Defining_Entity
                               (Enclosing_Declaration)) & "__" &
                          Namet.Get_Name_String
                          (Sinfo.Chars (Curr_Own_Var)));
--                       Curr_Symbol_Id := SPARK_Classic.Symbols.Intern
--                         (Sem_Util.Unique_Name
--                            (Sem_Util.Unique_Defining_Entity
--                                 (Enclosing_Declaration)) & "__" &
                     Namet.Get_Name_String
                       (Sinfo.Chars (Curr_Own_Var));
                  end if;
                  case Own_Mods'(Curr_Mod_Prop) is
                     when Plain =>
                        Put_Line ("Plain");
--                          Curr_Skind := SPARK_Classic.Symbols.SS_Own_Plain;
                     when External_In =>
                        Put_Line ("External_In");
--                          Curr_Skind :=
--                            SPARK_Classic.Symbols.SS_Own_External_In;
                     when External_Out =>
                        Put_Line ("External_Out");
--                          Curr_Skind :=
--                            SPARK_Classic.Symbols.SS_External_Out;
                     when Own_Task =>
                        Put_Line ("Own_Task");
--                          Curr_Skind :=
--                            SPARK_Classic.Symbols.SS_Own_Task;
                     when Protected_Own =>
                        Put_Line ("Protected_Own");
--                          Curr_Skind :=
--                            SPARK_Classic.Symbols.SS_Own_Protected;
                     when Protected_In =>
                        Put_Line ("Protected_In");
--                          Curr_Skind :=
--                            SPARK_Classic.Symbols.SS_Protected_In;
                     when Protected_Out =>
                        Put_Line ("Protected_Out");
--                          Curr_Skind :=
--                            SPARK_Classic.Symbols.SS_Protected_Out;
                  end case;
--                    if Pragma_Error = None then
--                       Curr_Symbol := Sym_With_Props;
--                       SPARK_Classic.Symbols.Set_Symbol_Kind
--                         (SK       => Curr_Skind,
--                          Sym_Node => Curr_Own_Var,
--                          SS       => Curr_Symbol);
--                       SPARK_Classic.Symbols.Insert
--                         (Key      => Curr_Symbol_Id,
--                          New_Item => Curr_Symbol,
--                          Inserted => Inserted);
--                       if not Inserted then
--                          Pragma_Error := Duplicate_Own_Item;
--                       end if;
--                    end if;
               else
                  Pragma_Error := Invalid_Own_Item;
               end if;
            else
               Pragma_Error := Invalid_Own_Modifier;
            end if;
         else
            Pragma_Error := Not_1_Choice;
         end if;
         if Pragma_Error = None then
            Curr_Pair := Nlists.Next (Curr_Pair);
         else
            Put_Line ("Pragma_Error: " &
                        Pragma_Errors'Image (Pragma_Error));
         end if;
      end loop;
   end Check_Own_Vars;

   procedure Check_Properties
     (Context        : SPARK_Classic.Contexts;
      Pairs          : Types.List_Id;
--        Sym_With_Props : in out SPARK_Classic.Symbols.Symbol;
      Pragma_Error   : out Pragma_Errors);
   --  # global SPARK_Classic.Symbols.Table;

   procedure Check_Properties
     (Context        : SPARK_Classic.Contexts;
      Pairs          : Types.List_Id;
--        Sym_With_Props : in out SPARK_Classic.Symbols.Symbol;
      Pragma_Error   : out Pragma_Errors)
   is
      Curr_Pair     : Types.Node_Id := Nlists.First (Pairs);
      Curr_Prop     : SPARK_Classic.Pragmas.Mod_Props;
      Curr_Value    : Types.Node_Id;
      Curr_Entity   : Types.Entity_Id;
      Curr_Etype    : Types.Entity_Id;
--        Curr_E_In_S   : Types.Entity_Id := Types.Empty;
      Int_Value     : Integer;
   begin
      Put_Line ("Check_Properties");
      if Context = SPARK_Classic.Classic_Own or
        Context = SPARK_Classic.Classic_Declare
      then
         Pragma_Error := None;
      else
         Pragma_Error := Properties_Not_Allowed;
      end if;

      while Pragma_Error = None and Atree.Present (Curr_Pair) loop
         Put_Line ("Get a pair");
         if Nlists.List_Length (Sinfo.Choices (Curr_Pair)) = 1 then
            Curr_Prop :=
              Mod_Prop_From_Node (Nlists.First (Sinfo.Choices (Curr_Pair)));
            if (Context = SPARK_Classic.Classic_Own and then
                Curr_Prop in SPARK_Classic.Pragmas.Own_Props) or else
              (Context = SPARK_Classic.Classic_Declare and then
               Curr_Prop in SPARK_Classic.Pragmas.Declare_Props)
            then
               Put_Line (Props'Image (Curr_Prop));
               Curr_Value := Sinfo.Expression (Curr_Pair);
               Put_Line ("Curr_Value");
               Print_Node_Briefly (Curr_Value);
               case Props'(Curr_Prop) is
                  when Own_Type =>
                     if Context = SPARK_Classic.Classic_Own then
                        if Atree.Nkind (Curr_Value) = Sinfo.N_Identifier
                          or else
                            Atree.Nkind (Curr_Value) =
                          Sinfo.N_Selected_Component
                        then
                           Put_Line ("Own_Type");
                           Print_Node_Briefly (Curr_Value);
                           Curr_Entity := SPARK_Classic.Pragmas.Atree_Utils.
                             Find_Entity_From_Name (Curr_Value);
                           Print_Node_Briefly (Curr_Value);
                           if Atree.Present (Curr_Entity) and then
                             Atree.Ekind (Curr_Entity) /= Einfo.E_Void
                           then
                              Print_Node_Briefly (Curr_Entity);
                              if Einfo.Is_Type (Curr_Entity) then
                                 Put_Line ("It is a type");
                                 Put_Line (Namet.Get_Name_String
                                           (Sinfo.Chars (Curr_Value)));
                              else
                                 Pragma_Error :=
                                   Name_Is_Not_A_Type_Mark;
                              end if;
                           else
                              --  The type may be declared later in the visible
                              --  part of the package or it may be a SPARK
                              --  abstract type.
                              --  The abstract type may be declared later in
                              --  the visible part of this package or
                              --  it may have been declared in another package.
                              Put_Line ("Not yet known");
                           end if;
                        else
                           Pragma_Error := Own_Type_Not_Applied_To_Own_Var;
                        end if;
                     else
                        Pragma_Error := Own_Type_Not_A_Name;
                     end if;
                  when Integrity | Priority =>
                     Put_Line ("Value?");
                     Put_Line
                       (Types.Int'Image
                          (Uintp.UI_To_Int
                               (Sem_Eval.Expr_Value (Curr_Value))));
                     Put_Line ("Is static expr: " &
                                 Boolean'Image
                                 (Sinfo.Is_Static_Expression
                                    (Curr_Value)));
                     Put_Line ("Is ok compile time: " &
                                 Boolean'Image
                                 (Sem_Eval.Compile_Time_Known_Value
                                    (Curr_Value)));
                     if Atree.Nkind (Curr_Value) in Sinfo.N_Has_Etype then
                        Curr_Etype :=
                          Sinfo.Etype (Curr_Value);
--                          Curr_E_In_S :=
--                      Sem_Util.Current_Entity_In_Scope (Curr_Value);
                        Put_Line ("Curr_Etype");
                        Print_Node_Briefly (Curr_Etype);
--                      Print_Node_Briefly (Sinfo.Scope (Curr_Entity));
--                      Print_Node_Briefly (Sem_Util.Current_Scope);
                     end if;
                     if Atree.Nkind (Curr_Value) = Sinfo.N_Integer_Literal then
                        Int_Value :=
                          Integer (Uintp.UI_To_Int
                                   (Sinfo.Intval (Curr_Value)));
                        Put_Line ("Lit Int_Value = " &
                                    Integer'Image (Int_Value));
--                       elsif Atree.Nkind (Curr_Value) in
--                         Sinfo.N_Binary_Op
--                       then
--                          Put_Line ("An operator");
--                          Print_Node_Briefly
--                            (Sinfo.Left_Opnd (Curr_Value));
--                          Print_Node_Briefly
--                            (Sinfo.Right_Opnd (Curr_Value));
                     elsif Sem_Eval.Is_OK_Static_Expression (Curr_Value) then
                        Put_Line ("Is static expression");
                        Curr_Entity :=
                          Sem_Util.Current_Entity (Curr_Value);
                        if Atree.Present (Curr_Entity) and then
                          Einfo.Is_Integer_Type (Sinfo.Etype (Curr_Entity))
                        then
                           Int_Value :=
                             Integer (Uintp.UI_To_Int
                                        (Sem_Eval.Expr_Value (Curr_Value)));
                        else
                           Pragma_Error := Not_Static_Integer_Expr;
                        end if;
                     else
                        Put_Line ("Rejected?");
                        Print_Node_Briefly (Curr_Value);
                        Pragma_Error := Not_Static_Integer_Expr;
                     end if;
                     if Pragma_Error = None then
                        Put_Line ("Int_Value = " &
                                    Integer'Image (Int_Value));

                        if Curr_Prop = Integrity then
                           if Int_Value not in Natural then
                              Pragma_Error := Integrity_Not_Natural;
                           end if;
                        end if;
                     end if;
                  when Interrupt | Protects | Suspends =>
                     null;
                  when May_Delay | Suspendable =>
                     null;
               end case;
--                 if not SPARK_Classic.Symbols.Has_Property
--                   (Curr_Prop, Sym_With_Props)
--                 then
--                    SPARK_Classic.Symbols.Set_Property
--                      (Property      => Curr_Prop,
--                       Property_Node => Curr_Value,
--                       SS            => Sym_With_Props);
--                 else
--                    Pragma_Error := Duplicate_Properties;
--                 end if;
            else
               Pragma_Error := Illegal_Property;
            end if;
         else
            Pragma_Error := Not_1_Choice;
         end if;
         if Pragma_Error = None then
            Curr_Pair := Nlists.Next (Curr_Pair);
         else
            Put_Line ("Pragma_Error: " &
                        Pragma_Errors'Image (Pragma_Error));
         end if;
      end loop;
   end Check_Properties;

--     procedure Print_Mod_Props_And_Values (Mod_Prop_List,
--                                           Value_List : Types.Elist_Id);
--
--     procedure Print_Mod_Props_And_Values (Mod_Prop_List,
--                                           Value_List : Types.Elist_Id)
--     is
--        Curr_Mod : Types.Elmt_Id :=
--          Elists.First_Elmt (Mod_Prop_List);
--        Curr_Val : Types.Elmt_Id :=
--          Elists.First_Elmt (Value_List);
--     begin
--        while Elists.Present (Curr_Mod) loop
--           Put_Line ("Mod_Prop_Value_Pair");
--           Print_Node_Briefly (Elists.Node (Curr_Mod));
--           Print_Node_Briefly (Elists.Node (Curr_Val));
--           if Atree.Nkind (Elists.Node (Curr_Val)) = Sinfo.N_Identifier then
--              Print_Node_Briefly
--                (Sem_Util.Current_Entity (Elists.Node (Curr_Val)));
--           elsif Atree.Nkind (Elists.Node (Curr_Val)) =
--             Sinfo.N_Selected_Component
--           then
--              Print_Node_Briefly
--                (Sem_Util.Current_Entity
--                   (Sinfo.Selector_Name (Elists.Node (Curr_Val))));
--           end if;
--           Put_Line
--             (Mod_Props'Image
--                (Mod_Prop_From_Node
--                     (Elists.Node (Curr_Mod))));
--           Curr_Mod := Elists.Next_Elmt (Curr_Mod);
--           Curr_Val  := Elists.Next_Elmt (Curr_Val);
--        end loop;
--     end Print_Mod_Props_And_Values;
--
--     procedure Print_Interrupt_Associations (Mod_Prop_List,
--                                            Value_List : Types.Elist_Id);
--
--     procedure Print_Interrupt_Associations (Mod_Prop_List,
--                                             Value_List : Types.Elist_Id)
--     is
--        Curr_Mod : Types.Elmt_Id :=
--          Elists.First_Elmt (Mod_Prop_List);
--        Curr_Val : Types.Elmt_Id :=
--          Elists.First_Elmt (Value_List);
--     begin
--        while Elists.Present (Curr_Mod) loop
--           Put_Line ("Interrupt association");
--           Print_Node_Briefly (Elists.Node (Curr_Mod));
--           Print_Node_Briefly (Elists.Node (Curr_Val));
--           if Atree.Nkind (Elists.Node (Curr_Mod)) = Sinfo.N_Identifier then
--              Print_Node_Briefly
--                (Sem_Util.Current_Entity (Elists.Node (Curr_Mod)));
--           elsif Atree.Nkind (Elists.Node (Curr_Mod)) =
--             Sinfo.N_Selected_Component
--           then
--              Print_Node_Briefly
--                (Sem_Util.Current_Entity
--                   (Sinfo.Selector_Name (Elists.Node (Curr_Mod))));
--           end if;
--           if Atree.Nkind (Elists.Node (Curr_Val)) = Sinfo.N_Identifier then
--              Print_Node_Briefly
--                (Sem_Util.Current_Entity (Elists.Node (Curr_Val)));
--           elsif Atree.Nkind (Elists.Node (Curr_Val)) =
--             Sinfo.N_Selected_Component
--           then
--              Print_Node_Briefly
--                (Sem_Util.Current_Entity
--                   (Sinfo.Selector_Name (Elists.Node (Curr_Val))));
--           end if;
--           Curr_Mod := Elists.Next_Elmt (Curr_Mod);
--           Curr_Val  := Elists.Next_Elmt (Curr_Val);
--        end loop;
--     end Print_Interrupt_Associations;

   procedure Own (N            : Types.Node_Id;
                  Pragma_Arg   : Types.Node_Id;
                  Arg_Sort     : Arg_Sorts;
                  Pragma_Error : out Pragma_Errors);

   -------------
   -- Process --
   -------------

   procedure Process (N : Types.Node_Id) is
      Prag_Context : constant SPARK_Classic.Ada_Contexts :=
        SPARK_Classic.Context_Of_Parent (N);
      Pname        : constant Namet.Name_Id := Sinfo.Pragma_Name (N);
      Prag_Id      : constant Snames.Classic_Annotation_Pragma :=
        Snames.Get_Pragma_Id (Pname);
--        Prag_Name    : constant Namet.Name_Id :=
--          Sem_Util.Original_Aspect_Pragma_Name (N);

      Parent : Types.Node_Id;
      Pragma_Error : Pragma_Errors;
      Applies_To : Types.Node_Id;
      Applies_Ctx : SPARK_Classic.Contexts;
      Applies_Entity : Types.Node_Id;
      Pragma_Arg : Types.Node_Id;
      Prag_Arg_Sort : Arg_Sorts;
   begin
      Put_Line ("***" & Namet.Get_Name_String (Sinfo.Pragma_Name (N)));
      Put_Line ("Parent");
      Parent := Atree.Parent (N);
      Print_Node_Briefly (Parent);
      Put_Line ("Context of node " &
                  SPARK_Classic.Contexts'Image
                  (SPARK_Classic.Context_Of_Node (N)));
      Put_Line ("Check_Placement");
      SPARK_Classic.Pragmas.Placement_Defns.Check_Placement
        (N              => N,
         Classic_Anno   => SPARK_Classic.Context_Of_Node (N),
         Parent_Context => Prag_Context,
         Applies_To     => Applies_To,
         Applies_Ctx    => Applies_Ctx,
         Applies_Entity => Applies_Entity,
         Pragma_Error   => Pragma_Error);
      Put_Line ("Pragma_Error: " &
                  Pragma_Errors'Image (Pragma_Error));
      Print_Node_Briefly (Applies_To);
      Print_Node_Briefly (Applies_Entity);
      Put_Line (SPARK_Classic.Contexts'Image
                  (Applies_Ctx));

      Put_Line ("pragma " & Namet.Get_Name_String (Sinfo.Pragma_Name (N)));
      Put_Line ("Ada context "
                & SPARK_Classic.Ada_Contexts'Image (Prag_Context));

      Get_Pragma_Argument
        (N            => N,
         Argument     => Pragma_Arg,
         Arg_Sort     => Prag_Arg_Sort,
         Pragma_Error => Pragma_Error);
      Put_Line ("Pragma argument");
      Print_Node_Briefly (Pragma_Arg);
      Put_Line (Arg_Sorts'Image (Prag_Arg_Sort));
      Put_Line (Pragma_Errors'Image (Pragma_Error));
      if Pragma_Error = None then
         case Prag_Id is
         when Snames.Pragma_Classic_Own =>
            Own (N, Pragma_Arg, Prag_Arg_Sort, Pragma_Error);
         when others =>
            Put_Line ("Pragma not yet handled");
         end case;
      else
         Put_Line (Pragma_Errors'Image (Pragma_Error));
      end if;
   end Process;

   procedure Old_Own (N : Types.Node_Id);

   procedure Own (N            : Types.Node_Id;
                  Pragma_Arg   : Types.Node_Id;
                  Arg_Sort     : Arg_Sorts;
                  Pragma_Error : out Pragma_Errors)
   is
      More_Specs     : Boolean;
      Curr_Spec      : Types.Node_Id;
--        Mod_Prop_List  : Types.Elist_Id;
--        Value_List     : Types.Elist_Id;
--        Sym_With_Props : SPARK_Classic.Symbols.Symbol;
   begin
      null;
      Pragma_Error := None;
      if Arg_Sort in Own_Variable_Arg_Sorts then
         if Arg_Sort = Expr_List then
            --  (own_variable_specification, own_variable_specification
            --     {, own_variable_specification})
            More_Specs := True;
            Curr_Spec := Nlists.First (Sinfo.Expressions (Pragma_Arg));
         else
            --  (own_variable_specification)
            More_Specs := False;
            Curr_Spec := Pragma_Arg;
         end if;
         loop
            Put_Line ("Spec sort");
--              Sym_With_Props := SPARK_Classic.Symbols.Null_Symbol;
            case Get_Arg_Sort (Curr_Spec) is
               when Comp_Assocs =>
                  Check_Own_Vars
                    (Pairs          =>
                        Sinfo.Component_Associations (Curr_Spec),
--                       Sym_With_Props => Sym_With_Props,
                     Pragma_Error   => Pragma_Error);
               when Single_Extension_Comp_Assocs =>
                  Put_Line ("Single_Extension_Comp_Assocs");
                  Pragma_Error := Wrong_Ancestor;
                  Put_Line (Pragma_Errors'Image (Pragma_Error));
               when Comp_Assocs_Extension_Comp_Assocs =>
                  Put_Line ("Comp_Assocs_Extension_Comp_Assocs");
                  Put_Line ("Extension component associations");
                  Check_Properties
                    (Context        => Context_Of_Node (N),
                     Pairs          =>
                        Sinfo.Component_Associations (Curr_Spec),
--                       Sym_With_Props => Sym_With_Props,
                     Pragma_Error   => Pragma_Error);
                  if Pragma_Error = None then
                     Check_Own_Vars
                       (Pairs          =>
                          Sinfo.Component_Associations
                            (Sinfo.Ancestor_Part (Curr_Spec)),
--                          Sym_With_Props => Sym_With_Props,
                        Pragma_Error   => Pragma_Error);
                  end if;

--                    Get_Mod_Props_And_Values
--                      (Pairs         =>
--                          Sinfo.Component_Associations (Curr_Spec),
--                       Mod_Prop_List => Mod_Prop_List,
--                       Value_List    => Value_List,
--                       Pragma_Error  => Pragma_Error);
--                    Print_Mod_Props_And_Values (Mod_Prop_List, Value_List);
--                    declare
--                       Curr_Mod_Prop : Types.Elmt_Id :=
--                         Elists.First_Elmt (Mod_Prop_List);
--                       Curr_Value    : Types.Elmt_Id :=
--                         Elists.First_Elmt (Value_List);
--                    begin
--                       Put_Line ("Extension arguments");
--                       while Elists.Present (Curr_Mod_Prop) loop
--                          Put_Line
--                            (Mod_Props'Image
--                               (Mod_Prop_From_Node
--                                    (Elists.Node (Curr_Mod_Prop))));
--                          Print_Node_Briefly (Elists.Node (Curr_Value));
--                          if Atree.Nkind (Elists.Node (Curr_Value)) =
--                            Sinfo.N_Identifier
--                          then
--                             Print_Node_Briefly
--                               (Sem_Util.Current_Entity
--                                  (Elists.Node (Curr_Value)));
--                          elsif Atree.Nkind (Elists.Node (Curr_Value)) =
--                            Sinfo.N_Selected_Component
--                          then
--                             Print_Node_Briefly
--                               (Sem_Util.Current_Entity
--                                  (Sinfo.Selector_Name
--                                       (Elists.Node (Curr_Value))));
--                          elsif Mod_Prop_From_Node
--                            (Elists.Node (Curr_Mod_Prop)) =
--                            Interrupt
--                          then
--                             declare
--                                Interrupt_Handler : Types.Elist_Id;
--                                Interrupt_Source  : Types.Elist_Id;
--                             begin
--                                Put_Line ("An interrupt");
--                                Get_Interrupt_Associations
--                                  (Associations => Elists.Node (Curr_Value),
--                                   Handler_List => Interrupt_Handler,
--                                   Source_List  => Interrupt_Source,
--                                   Pragma_Error => Pragma_Error);
--                                if Pragma_Error = None then
--                                   Print_Interrupt_Associations
--                                     (Interrupt_Handler, Interrupt_Source);
--                                else
--                              Put_Line (Pragma_Errors'Image (Pragma_Error));
--                                end if;
--                             end;
--                          else
--                             Print_Node_Subtree (Elists.Node (Curr_Value));
--                          end if;
--                          Curr_Mod_Prop := Elists.Next_Elmt (Curr_Mod_Prop);
--                          Curr_Value := Elists.Next_Elmt (Curr_Value);
--                       end loop;
--                    end;
--                    if Get_Aggregate_Sort (Sinfo.Ancestor_Part (Curr_Spec)) =
--                      Comp_Assocs
--                    then
--                       Get_Mod_Props_And_Values
--                         (Pairs         =>
--                            Sinfo.Component_Associations
--                              (Sinfo.Ancestor_Part (Curr_Spec)),
--                          Mod_Prop_List => Mod_Prop_List,
--                          Value_List    => Value_List,
--                          Pragma_Error  => Pragma_Error);
--                     Print_Mod_Props_And_Values (Mod_Prop_List, Value_List);
--                    else
--                       Pragma_Error := Wrong_Ancestor;
--                       Put_Line (Pragma_Errors'Image (Pragma_Error));
--                    end if;
               when Ancestor_Extension =>
                  null;
                  Pragma_Error := Ancestor_Part_Is_Extension;
                  Put_Line ("Pragma_Error");
                  Put_Line (Pragma_Errors'Image (Pragma_Error));
               when Mixed_Pos_And_Named =>
                  null;
                  Pragma_Error := Mixed_Positional_And_Named;
                  Put_Line ("Pragma_Error");
                  Put_Line (Pragma_Errors'Image (Pragma_Error));
               when others =>
                  Pragma_Error := Not_A_Comp_Assoc_Or_Extension;
                  Put_Line ("Pragma_Error");
                  Put_Line (Pragma_Errors'Image (Pragma_Error));
            end case;
            exit when not More_Specs;
            --  There is at least 1 more specification.
            Curr_Spec := Nlists.Next (Curr_Spec);
            More_Specs := Atree.Present (Nlists.Next (Curr_Spec));
         end loop;

         Old_Own (N);
      else
         Put_Line ("Expected a Own_Variable_Arg_Sort got");
         Put_Line (Arg_Sorts'Image (Arg_Sort));
         Print_Node_Briefly (Pragma_Arg);
         Pragma_Error := Not_An_Aggregate;
      end if;
   end Own;

   procedure Old_Own (N     : Types.Node_Id) is
      Enclosing_Dec     : Types.Node_Id;
      From_Aspect       : Types.Node_Id;
      Entity_Of_Aspect  : Types.Node_Id;
      Aggregate         : Types.Node_Id;
      Agg_Sort          : Aggregate_Sort;
      Ancestor_Agg      : Types.Node_Id;
      Pragma_Error      : Pragma_Errors;
      Own_Var_Spec_List : Types.List_Id;
      Own_Var_Spec      : Types.Node_Id;
      Modified_Own_Var  : Types.Node_Id;
      Own_Var           : Types.Node_Id;
      Modifier          : Modifiers;
      Multi_Specs       : Boolean;
      Continue_Looping  : Boolean;
      Property          : Types.Node_Id;
      Property_Name     : Properties;
      Property_Expr     : Types.Node_Id;
      Property_Value    : Types.Node_Id;
      Interrupt_Assoc   : Types.Node_Id;
      Interrupt_Handler : Types.Node_Id;
      Interrupt_Source  : Types.Node_Id;
      Arg_Sort          : Arg_Sorts;
   begin
      Pragma_Error := None;
      Put_Line ("Own variable declaration");
      Print_Node_Subtree (N);
      --  An own variable annotation/pragma can only be
      --  applied to a package declaration.
      Put_Line ("enclosing declaration");
      Enclosing_Dec := Sem_Util.Enclosing_Declaration (N);
      Print_Node_Briefly (Enclosing_Dec);
      if Sinfo.From_Aspect_Specification (N) then
         From_Aspect := Sinfo.Corresponding_Aspect (N);
         Put_Line ("Corresponding Aspect");
         Print_Node_Briefly (From_Aspect);
         Entity_Of_Aspect := Sinfo.Entity (From_Aspect);
         if Atree.Present (Entity_Of_Aspect) then
            Put_Line ("Declaration node " &
                        Sinfo.Node_Kind'Image
                        (Atree.Nkind
                           (Einfo.Declaration_Node (Entity_Of_Aspect))));
         end if;
      end if;
      if Atree.Nkind (Enclosing_Dec) /= Sinfo.N_Package_Declaration and then
        Atree.Nkind (Enclosing_Dec) /= Sinfo.N_Generic_Package_Declaration
      then
         Put_Line
           ("own variable annotation/pragma not in package_declaration");
         Pragma_Error := Own_Not_In_Package_Dec;

         --  An own variable annotation must be placed before the "is"
         --  and the corresponding own variable pragma must be the
         --  first item in the declarative part of the package declaration.
      elsif Sinfo.From_Aspect_Specification (N) then
         From_Aspect := Sinfo.Corresponding_Aspect (N);
         Put_Line ("Corresponding Aspect");
         Print_Node_Briefly (From_Aspect);
         Entity_Of_Aspect := Sinfo.Entity (From_Aspect);
         if Atree.Present (Entity_Of_Aspect) then
            Put_Line ("Declaration node " &
                        Sinfo.Node_Kind'Image
                        (Atree.Nkind
                           (Einfo.Declaration_Node (Entity_Of_Aspect))));
            case Atree.Nkind (Einfo.Declaration_Node (Entity_Of_Aspect)) is
               when Sinfo.N_Package_Specification |
                    Sinfo.N_Package_Declaration |
                    Sinfo.N_Generic_Package_Declaration =>
                  null;
               when others =>
                  Put_Line
                    ("own variable annotation not in package_declaration");
                  Print_Node_Briefly
                    (Einfo.Declaration_Node (Entity_Of_Aspect));
                  Pragma_Error := Own_Not_In_Package_Dec;
            end case;
         end if;

         if Pragma_Error =  None and then
           Nlists.Is_List_Member (From_Aspect) and then
           Atree.Present (Nlists.Prev (From_Aspect))
         then
            --  As the pragma arises from an aspect, in SPARK_Classic,
            --  it must be from an annotation but it is not the first
            --  annotation.  No annotation should precede an own annotation
            --  in a package specification.
            Put_Line ("annotation");
            Put_Line ("Not first aspect");
            Put_Line (Sinfo.Node_Kind'Image
                      (Atree.Nkind (Nlists.Prev (From_Aspect))));
            if Atree.Nkind (Nlists.Prev (From_Aspect)) =
              Sinfo.N_Aspect_Specification
            then
               Put_Line (Namet.Get_Name_String
                         (Sinfo.Chars
                            (Sinfo.Identifier (Nlists.Prev (From_Aspect)))));
            end if;
            Pragma_Error := Misplaced_Annotation;
         end if;
      elsif Nlists.Is_List_Member (N) and then
        Atree.Present (Nlists.Prev (N))
      then
         declare
            Iter : Types.Node_Id := Nlists.Prev (N);
         begin
            Put_Line ("Previous");
            while Atree.Present (Iter) loop
               Print_Node_Briefly (Iter);
               if Atree.Nkind (Iter) = Sinfo.N_Pragma then
                  Put_Line
                    (Namet.Get_Name_String (Sinfo.Pragma_Name (Iter)));
               end if;
               Iter := Nlists.Prev (Iter);
            end loop;
            Iter := Nlists.Next (N);
            Put_Line ("Next");
            while Atree.Present (Iter) loop
               Print_Node_Briefly (Iter);
               if Atree.Nkind (Iter) = Sinfo.N_Pragma then
                  Put_Line
                    (Namet.Get_Name_String (Sinfo.Pragma_Name (Iter)));
               end if;
               Iter := Nlists.Next (Iter);
            end loop;
         end;
         Put_Line ("misplaced pragma");
         Pragma_Error := Misplaced_Annotation;
      end if;

      if Pragma_Error = None then
         Get_Pragma_Argument
           (N            => N,
            Argument     => Aggregate,
            Arg_Sort     => Arg_Sort,
            Pragma_Error => Pragma_Error);

         Put_Line (Arg_Sorts'Image (Arg_Sort));
         Print_Node_Briefly (Aggregate);
      end if;

      if Pragma_Error = None then
         Agg_Sort := Get_Aggregate_Sort (Aggregate);
      end if;

      --  Check for multiple own variable specifications
      if Pragma_Error = None then
         if Agg_Sort = Exprs
         then
            Put_Line ("Multiple own_variable_Specfications");
            Own_Var_Spec_List := Sinfo.Expressions (Aggregate);
            Own_Var_Spec := Nlists.First (Own_Var_Spec_List);
            Multi_Specs := True;
         else
            Put_Line ("Single own_variable_specification");
            --  There is anly a single own_variable_Specification
            Multi_Specs := False;
            Own_Var_Spec := Aggregate;
         end if;
      end if;

      Continue_Looping := Pragma_Error = None;
      while Continue_Looping loop
         Put_Line ("own_variable_specification");
         Print_Node_Briefly (Own_Var_Spec);
         --  The own_variable_specification must be an aggregate
         if Atree.Nkind (Own_Var_Spec) /= Sinfo.N_Aggregate and
           Atree.Nkind (Own_Var_Spec) /= Sinfo.N_Extension_Aggregate
         then
            Pragma_Error := Not_List_Of_Aggregates;
         end if;

         if Pragma_Error = None then
            --  An own_variable_specification can take the form of a
            --  Series of component associations or
            --  a record extension whse ancestor part is a series of
            --  component associations.
            --  In either case the each of the component associations
            --  is a modified_own_variable.
            --  Get the first in the series.
            case Get_Aggregate_Sort (Own_Var_Spec) is
            when Comp_Assocs =>
               Put_Line ("Comp Assocs");
               Modified_Own_Var :=
                 Nlists.First (Sinfo.Component_Associations (Own_Var_Spec));
            when Extension =>
               Put_Line ("Extension");
               Ancestor_Agg := Sinfo.Ancestor_Part (Own_Var_Spec);
               Print_Node_Briefly (Ancestor_Agg);
               if Atree.Nkind (Ancestor_Agg) = Sinfo.N_Aggregate and then
                 Get_Aggregate_Sort (Ancestor_Agg) = Comp_Assocs
               then
                  Modified_Own_Var :=
                    Nlists.First (Sinfo.Component_Associations (Ancestor_Agg));
               else
                  Pragma_Error := Wrong_Ancestor;
               end if;
            when Exprs =>
               Pragma_Error := Expr_List_Not_Allowed;
            when Mixed =>
               Pragma_Error := Expr_List_Not_Allowed;
            when Unknown =>
               Pragma_Error := Wrong_Aggregate;
            end case;

            --  Now iterate through the modified_own_variables
            while
              Pragma_Error = None and then Atree.Present (Modified_Own_Var)
            loop
               Get_Modifier (Modified_Own_Var, Modifier, Pragma_Error);
               Own_Var := Sinfo.Expression (Modified_Own_Var);
               Put_Line
                 ("modified_own_variable " & Modifiers'Image (Modifier));
               Print_Node_Briefly (Own_Var);
               Print_Node_Briefly (Sem_Util.Current_Entity (Own_Var));
               Modified_Own_Var := Nlists.Next (Modified_Own_Var);
            end loop;
            if Pragma_Error = None and then
              Get_Aggregate_Sort (Own_Var_Spec) = Extension
            then
                  Put_Line ("Has_Properties");
               Property :=
                 Nlists.First (Sinfo.Component_Associations (Own_Var_Spec));
               while Pragma_Error = None and then Atree.Present (Property) loop
                  Get_Property (Property, Property_Name, Pragma_Error);
                  Property_Expr := Sinfo.Expression (Property);
                  Put_Line ("property " & Properties'Image (Property_Name));
                  Print_Node_Briefly (Property_Expr);
                  if Pragma_Error = None and then
                    Property_Name in Name_Only_Properties
                  then
                     if Atree.Nkind (Property_Expr) = Sinfo.N_Identifier then
                        if Atree.Present (Sem_Util.Current_Entity
                                          (Property_Expr))
                        then
                           Put_Line (Einfo.Entity_Kind'Image
                                     (Atree.Ekind
                                        (Sem_Util.Current_Entity
                                           (Property_Expr))));
                        end if;

                        if Has_Chars (Property_Expr, "true") then
                           Put_Line ("TRUE");
                        else
                           Pragma_Error := Name_Only_Property_Not_True;
                        end if;
                     end if;
                  elsif Atree.Nkind (Property_Expr) /= Sinfo.N_Aggregate then
                     Put_Line ("simple expression");
                     Print_Node_Briefly (Property_Expr);
                     if Atree.Nkind (Property_Expr) in Sinfo.N_Has_Entity then
                        Print_Node_Briefly (Sem_Util.Current_Entity
                                         (Property_Expr));
                        if Atree.Present (Sem_Util.Current_Entity
                                          (Property_Expr))
                        then
                           Put_Line (Einfo.Entity_Kind'Image
                                     (Atree.Ekind
                                        (Sem_Util.Current_Entity
                                           (Property_Expr))));
                        end if;
                     end if;
                  else
                     case Get_Aggregate_Sort (Property_Expr) is
                     when Exprs =>
                        Put_Line ("protected elements");
                        Property_Value :=
                          Nlists.First (Sinfo.Expressions (Property_Expr));
                        while Atree.Present (Property_Value) loop
                           Print_Node_Briefly (Property_Value);
                           Property_Value := Nlists.Next (Property_Value);
                        end loop;
                     when Comp_Assocs =>
                        Put_Line ("interrupt associations");
                        Interrupt_Assoc :=
                          Nlists.First
                            (Sinfo.
                               Component_Associations (Property_Expr));
                        while Pragma_Error = None and then
                          Atree.Present (Interrupt_Assoc)
                        loop
                           Get_Single_Assoc_Choice
                             (Interrupt_Assoc, Interrupt_Handler,
                              Pragma_Error);
                           if Pragma_Error = None then
                              Put_Line ("interrupt_handler");
                              Print_Node_Briefly (Interrupt_Handler);
                              Put_Line ("interrupt_source");
                              Interrupt_Source :=
                                Sinfo.Expression (Interrupt_Assoc);
                              if Atree.Nkind (Interrupt_Source) =
                                Sinfo.N_Identifier
                              then
                                 Print_Node_Briefly (Interrupt_Source);
--                                elsif Atree.Nkind (Interrupt_Source) =
--                                  Sinfo.N_Selected_Component
--                                then
--
--                                   while Atree.Nkind
--                                     (Sinfo.Expression (Interrupt_Assoc)) =
--                                       Sinfo.N_Selected_Component
--                                   loop
--                                      null;
--                                   end loop;
                              else
                                 Print_Node_Briefly
                                   (Sinfo.Expression (Interrupt_Assoc));
                                 Pragma_Error := Illegal_Interrupt_Source;
                              end if;
                              Interrupt_Assoc := Nlists.Next (Interrupt_Assoc);
                           end if;
                        end loop;
                     when others =>
                        Pragma_Error := Wrong_Property_Aggregate;
                     end case;
                  end if;
                  Property := Nlists.Next (Property);
               end loop;
            end if;

            if Pragma_Error = None and Multi_Specs then
               Own_Var_Spec := Nlists.Next (Own_Var_Spec);
               Continue_Looping := Atree.Present (Own_Var_Spec);
            else
               Continue_Looping := False;
            end if;
         else
            Continue_Looping := False;
         end if;
      end loop;
      if Pragma_Error = None then
         Put_Line ("Done ok");
      else
         Put_Line ("Pragma_Error " & Pragma_Errors'Image (Pragma_Error));
      end if;
   end Old_Own;

end SPARK_Classic.Pragmas;
