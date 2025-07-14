with Types,
     Namet,
     Atree,
     Nlists,
     Sinfo;
use type Types.List_Id, Sinfo.Node_Kind;
package body SPARK_Classic is

   -----------------------
   -- Context_Of_Node   --
   -----------------------

   function Context_Of_Node (N : Types.Node_Id) return Contexts is
      Kind_Of_Node : constant Sinfo.Node_Kind := Atree.Nkind (N);
      Name         : Namet.Name_Id;
      Result       : Contexts;
   begin
      if Kind_Of_Node = Sinfo.N_Pragma or
        Kind_Of_Node = Sinfo.N_Aspect_Specification
      then
         if Kind_Of_Node = Sinfo.N_Pragma then
            Name := Sinfo.Pragma_Name (N);
         else
            Name := Sinfo.Chars (Sinfo.Identifier (N));
         end if;
         if Name in Snames.Classic_Annotation then
            Result := Name_To_Annotation (Name);
         else
            Result := Blank_Context;
         end if;
      else
         case Kind_Of_Node is
            when Sinfo.N_Generic_Package_Declaration =>
               Result := Generic_Package_Declaration;
            when Sinfo.N_Generic_Subprogram_Declaration =>
               if Atree.Nkind (Sinfo.Specification (N)) =
                 Sinfo.N_Procedure_Specification
               then
                  Result := Generic_Procedure_Declaration;
               else
                  Result := Generic_Function_Declaration;
               end if;
            when Sinfo.N_Package_Declaration =>
               Result := Package_Declaration;
            when Sinfo.N_Package_Specification =>
               Result := Package_Specification;
            when Sinfo.N_Subprogram_Declaration =>
               if Atree.Nkind (Sinfo.Specification (N)) =
                 Sinfo.N_Procedure_Specification
               then
                  Result := Procedure_Declaration;
               else
                  Result := Function_Declaration;
               end if;
            when Sinfo.N_Procedure_Specification =>
               Result := Procedure_Specification;
            when Sinfo.N_Function_Specification =>
               Result := Function_Specification;
            when Sinfo.N_Package_Body =>
               Result := Package_Body;
            when Sinfo.N_Subprogram_Body =>
               if Atree.Nkind (Sinfo.Specification (N)) =
                 Sinfo.N_Procedure_Specification
               then
                  Result := Procedure_Body;
               else
                  Result := Function_Body;
               end if;
            when others =>
               Result := Blank_Context;
         end case;
      end if;
      return Result;
   end Context_Of_Node;

   -----------------------
   -- Context_Of_Parent --
   -----------------------

   function Context_Of_Parent (N : Types.Node_Id) return Ada_Contexts is
      Parent_Node   : constant Types.Node_Id := Atree.Parent (N);
      List_Of_Node  : Types.List_Id;
      Result        : Ada_Contexts;
   begin
      case Atree.Nkind (Parent_Node) is
         when Sinfo.N_Package_Specification =>
            --  The parent of an entity or pragma ocurring immediateltly
            --  within a package or generic package declaration will be
            --  a package specification.
            --  It may be placed in the visible or private parts.
            if Nlists.Is_List_Member (N) then
               List_Of_Node := Nlists.List_Containing (N);
               if List_Of_Node = Sinfo.Visible_Declarations (Parent_Node) then
                  Result := Package_Visible_Part;
               elsif List_Of_Node = Sinfo.Private_Declarations (Parent_Node)
               then
                  Result := Package_Private_Part;
               else
                  --  The entity does not seem to be in the visible or private
                  --  parts - it must be erroneous.
                  Result := Blank_Context;
               end if;
            else
               --  The package has no visible or private declarations
               --  the given node must be erroneous.
               Result := Blank_Context;
            end if;
         when Sinfo.N_Package_Body =>
            Result := Package_Body_Declarative_Part;
         when Sinfo.N_Subprogram_Body =>
            if Atree.Nkind (Sinfo.Specification (Parent_Node)) =
              Sinfo.N_Procedure_Specification
            then
               Result := Procedure_Declarative_Part;
            else
               Result := Function_Declarative_Part;
            end if;
         when Sinfo.N_Block_Statement =>
            Result := Block_Declarative_Part;
         when Sinfo.N_Handled_Sequence_Of_Statements =>
            --  If the parent of the entity or pragma is a sequence of
            --  statements the parent of this sequence is required to
            --  determine the context of the entity or pragma.
            case Atree.Nkind (Atree.Parent (Parent_Node)) is
               --  If the parent of the sequence is a subprogram body
               --  is it a procedure or a function?
               when Sinfo.N_Subprogram_Body =>
                  if Atree.Nkind (Sinfo.Specification
                                  (Atree.Parent (Parent_Node))) =
                    Sinfo.N_Procedure_Specification
                  then
                     Result := Procedure_Statements;
                  else
                     Result := Function_Statements;
                  end if;
               when Sinfo.N_Package_Body =>
                  Result := Package_Statements;
               when Sinfo.N_Block_Statement =>
                  Result := Block_Statements;
               when others =>
                  Result := Blank_Context;
            end case;
         when others =>
            Result := Blank_Context;
      end case;
      return Result;
   end Context_Of_Parent;

   ------------------------------
   -- Context_From_Declaration --
   ------------------------------

   function Context_From_Declaration (Dec : Types.Node_Id)
                                      return Ada_Contexts
   is
      Result : Ada_Contexts;
   begin
      case Atree.Nkind (Dec) is
         when Sinfo.N_Package_Specification   =>
            Result := Package_Specification;
         when Sinfo.N_Procedure_Specification =>
            Result := Procedure_Specification;
         when Sinfo.N_Function_Specification  =>
            Result := Function_Specification;
         when others =>
            Result := Blank_Context;
      end case;
      return Result;
   end Context_From_Declaration;

end SPARK_Classic;
