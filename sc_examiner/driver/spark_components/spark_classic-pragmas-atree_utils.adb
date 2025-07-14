with Namet, Einfo, Sem, Sem_Ch8, Sem_Type, Sem_Util, Sem_Dist,
     Stand, Restrict, Opt, Tbuild, Lib.Xref;
use type Namet.Name_Id, Einfo.Entity_Kind, Opt.Ada_Version_Type;
package body SPARK_Classic.Pragmas.Atree_Utils is

   procedure Set_Entity_Or_Discriminal (N : Types.Node_Id;
                                        E : Types.Entity_Id);
   --  Set Entity, with style check if need be. For a discriminant reference,
   --  replace by the corresponding discriminal, i.e. the parameter of the
   --  initialization procedure that corresponds to the discriminant.

   procedure Set_Entity_Or_Discriminal (N : Types.Node_Id;
                                        E : Types.Entity_Id) is
      P : Types.Node_Id;

   begin
      --  If the entity is not a discriminant, or else expansion is disabled,
      --  simply set the entity.

      if not Sem.In_Spec_Expression
        or else Atree.Ekind (E) /= Einfo.E_Discriminant
        or else Sem.Inside_A_Generic
      then
         Sem_Util.Set_Entity_With_Checks (N, E);

      --  The replacement of a discriminant by the corresponding discriminal
      --  is not done for a task discriminant that appears in a default
      --  expression of an entry parameter. See Exp_Ch2.Expand_Discriminant
      --  for details on their handling.

      elsif Einfo.Is_Concurrent_Type (Sinfo.Scope (E)) then
         P := Atree.Parent (N);
         while Atree.Present (P)
           and then Atree.Nkind (P) not in
           Sinfo.N_Parameter_Specification | Sinfo.N_Component_Declaration
         loop
            P := Atree.Parent (P);
         end loop;

         if Atree.Present (P)
           and then Atree.Nkind (P) = Sinfo.N_Parameter_Specification
         then
            null;

         else
            Sinfo.Set_Entity (N, Einfo.Discriminal (E));
         end if;

         --  Otherwise, this is a discriminant in a context in which
         --  it is a reference to the corresponding parameter of the
         --  init proc for the enclosing type.

      else
         Sinfo.Set_Entity (N, Einfo.Discriminal (E));
      end if;
   end Set_Entity_Or_Discriminal;

   function Find_Entity_From_Direct_Name (N : Types.Node_Id)
                                          return Types.Entity_Id;

   function Find_Entity_From_Direct_Name (N : Types.Node_Id)
                                          return Types.Entity_Id
   is
      E    : Types.Entity_Id;
      E2   : Types.Entity_Id;
      Level : Types.Int;
      Scop  : Types.Entity_Id;
      Result : Types.Entity_Id;

      Homonyms : Types.Entity_Id;
      --  Saves start of homonym chain

      function Is_Actual_Parameter return Boolean;
      --  This function checks if the node N is an identifier that is an actual
      --  parameter of a procedure call. If so it returns True, otherwise it
      --  return False. The reason for this check is that at this stage we do
      --  not know what procedure is being called if the procedure might be
      --  overloaded, so it is premature to go setting referenced flags or
      --  making calls to Generate_Reference. We will wait till Resolve_Actuals
      --  for that processing

      -------------------------
      -- Is_Actual_Parameter --
      -------------------------

      function Is_Actual_Parameter return Boolean is
      begin
         return
           Atree.Nkind (N) = Sinfo.N_Identifier
           and then
             (Atree.Nkind (Atree.Parent (N)) = Sinfo.N_Procedure_Call_Statement
              or else
                (Atree.Nkind (Atree.Parent (N)) = Sinfo.N_Parameter_Association
                 and then N = Sinfo.Explicit_Actual_Parameter
                   (Atree.Parent (N))
                 and then Atree.Nkind (Atree.Parent (Atree.Parent (N))) =
                     Sinfo.N_Procedure_Call_Statement));
      end Is_Actual_Parameter;

      --  Start of processing for Find_Entity_From_Direct_Name

   begin
      --  If the entity pointer is already set, this is an internal node, or
      --  a node that is analyzed more than once, after a tree modification.
      --  In such a case there is no resolution to perform, just set the type.

      if Atree.Present (Sinfo.Entity (N)) then
         if Einfo.Is_Type (Sinfo.Entity (N)) then
            Sinfo.Set_Etype (N, Sinfo.Entity (N));

         else
            declare
               Entyp : constant Types.Entity_Id :=
                 Sinfo.Etype (Sinfo.Entity (N));

            begin
               --  One special case here. If the Etype field is already set,
               --  and references the packed array type corresponding to the
               --  etype of the referenced entity, then leave it alone. This
               --  happens for trees generated from Exp_Pakd, where expressions
               --  can be deliberately "mis-typed" to the packed array type.

               if Einfo.Is_Array_Type (Entyp)
                 and then Einfo.Is_Packed (Entyp)
                 and then Atree.Present (Sinfo.Etype (N))
                 and then Sinfo.Etype (N) =
                 Einfo.Packed_Array_Impl_Type (Entyp)
               then
                  null;

                  --  If not that special case, then just reset the Etype

               else
                  Sinfo.Set_Etype (N, Sinfo.Etype (Sinfo.Entity (N)));
               end if;
            end;
            Result := Sinfo.Entity (N);
         end if;
      else
         --  Here if Entity pointer was not set,
         --  we need full visibility analysis

         Homonyms := Sem_Util.Current_Entity (N);

         E := Homonyms;
         --  If entity is immediately visible then
         --  process the entity and we are done.
         while Atree.Present (E) and then not Einfo.Is_Immediately_Visible (E)
         loop
            --  Move to next entity in chain and continue search
            E := Einfo.Homonym (E);
         end loop;

         if Atree.Present (E) then
            --  An immediately visible entity has been found.
            --  Find scope level of initial entity. When compiling through
            --  Rtsfind, the previous context is not completely invisible, and
            --  an outer entity may appear on the chain, whose scope is below
            --  the entry for Standard that delimits the current scope stack.
            --  Indicate that the level for this spurious entry is outside of
            --  the current scope stack.

            Level := Sem.Scope_Stack.Last;
            loop
               Scop := Sem.Scope_Stack.Table (Level).Entity;
               exit when Scop = Sinfo.Scope (E);
               Level := Level - 1;
               exit when Scop = Stand.Standard_Standard;
            end loop;

            --  Now search remainder of homonym chain for more inner entry
            --  If the entity is Standard itself, it has no scope, and we
            --  compare it with the stack entry directly.

            E2 := Einfo.Homonym (E);
            while Atree.Present (E2) loop
               if Einfo.Is_Immediately_Visible (E2) then

                  --  If a generic package contains a local declaration that
                  --  has the same name as the generic, there may be a
                  --  visibility conflict in an instance, where the local
                  --  declaration must also hide the name of the corresponding
                  --  package renaming. We check explicitly for a package
                  --  declared by a renaming, whose renamed entity is an
                  --  instance that is on the scope stack, and that contains a
                  --  homonym in the same scope. Once we have found it,
                  --  we know that the package renaming is not
                  --  immediately visible, and that the identifier denotes the
                  --  other entity (and its homonyms if overloaded).

                  if Sinfo.Scope (E) = Sinfo.Scope (E2)
                    and then Atree.Ekind (E) = Einfo.E_Package
                    and then Atree.Present (Einfo.Renamed_Object (E))
                    and then Einfo.Is_Generic_Instance
                      (Einfo.Renamed_Object (E))
                    and then Sem_Ch8.In_Open_Scopes (Einfo.Renamed_Object (E))
                    and then Atree.Comes_From_Source (N)
                  then
                     Einfo.Set_Is_Immediately_Visible (E, False);
                     E := E2;

                  else
                     for J in Level + 1 .. Sem.Scope_Stack.Last loop
                        if Sem.Scope_Stack.Table (J).Entity = Sinfo.Scope (E2)
                          or else Sem.Scope_Stack.Table (J).Entity = E2
                        then
                           Level := J;
                           E := E2;
                           exit;
                        end if;
                     end loop;
                  end if;
               end if;

               E2 := Einfo.Homonym (E2);
            end loop;

            --  At the end of that loop, E is the innermost immediately
            --  visible entity, so we are all set.

            --  Come here with entity found, and stored in E

            --  Check violation of No_Wide_Characters restriction

            Restrict.Check_Wide_Character_Restriction (E, N);

            --  When distribution features are available (Get_PCS_Name /=
            --  Name_No_DSA), a remote access-to-subprogram type is converted
            --  into a record type holding whatever information is needed to
            --  perform a remote call on an RCI subprogram. In that case we
            --  rewrite any occurrence of the RAS type into the equivalent
            --  record type here. 'Access attribute references and RAS
            --  dereferences are then implemented using specific TSSs.
            --  However when distribution is not available
            --  (case of Get_PCS_Name = Name_No_DSA), we bypass the
            --  generation of these TSSs, and we must keep the RAS type in its
            --  original access-to-subprogram form (since all calls through a
            --  value of such type will be local anyway in the absence of a
            --  PCS).

            if Atree.Comes_From_Source (N)
              and then Sem_Util.Is_Remote_Access_To_Subprogram_Type (E)
              and then Atree.Ekind (E) = Einfo.E_Access_Subprogram_Type
              and then Opt.Expander_Active
              and then Sem_Dist.Get_PCS_Name /= Snames.Name_No_DSA
            then
               Atree.Rewrite
                 (N,
                  Tbuild.New_Occurrence_Of
                    (Einfo.Equivalent_Type (E), Atree.Sloc (N)));
            else
               --  Set the entity. Note that the reason we call Set_Entity for
               --  the overloadable case, as opposed to Set_Entity_With_Checks
               --  is that in the overloaded case, the initial call can set
               --  the wrong homonym. The call that sets the right homonym is
               --  in Sem_Res and that call does use Set_Entity_With_Checks,
               --  so we don't miss a style check.

               if Einfo.Is_Overloadable (E) then
                  Sinfo.Set_Entity (N, E);
               else
                  Sem_Util.Set_Entity_With_Checks (N, E);
               end if;

               if Einfo.Is_Type (E) then
                  Sinfo.Set_Etype (N, E);
               else
                  Sinfo.Set_Etype (N, Einfo.Get_Full_View (Sinfo.Etype (E)));
               end if;

               --  If the Ekind of the entity is Void, it means that all
               --  homonyms are hidden from all visibility (RM 8.3(5,14-20)).
               --  However, this test is skipped if the current scope is a
               --  record and the name is a pragma argument expression
               --  (case of Atomic and Volatile pragmas and possibly other
               --  similar pragmas added later, which are allowed
               --  to reference components in the current record).

               if Atree.Ekind (E) = Einfo.E_Void
                 and then
                   (not Einfo.Is_Record_Type (Sem_Util.Current_Scope)
                    or else Atree.Nkind
                      (Atree.Parent (N)) /=
                        Sinfo.N_Pragma_Argument_Association)
               then
                  --  Within an instance, the analysis of the actual for a
                  --  formal object does not see the name of the object
                  --  itself. This is significant only if the object is an
                  --  aggregate, where its analysis does not do any name
                  --  resolution on component associations. (see 4717-008).
                  --  In such a case, look for the visible homonym on the
                  --  chain.
                  if Sem_Util.In_Instance and then
                    Atree.Present (Einfo.Homonym (E))
                  then
                     E := Einfo.Homonym (E);
                     while Atree.Present (E) and then
                       not Sem_Ch8.In_Open_Scopes (Sinfo.Scope (E)) loop
                        E := Einfo.Homonym (E);
                     end loop;

                     if Atree.Present (E) then
                        Sinfo.Set_Entity (N, E);
                        Sinfo.Set_Etype (N, Sinfo.Etype (E));
                     end if;
                  end if;

                  --  If the entity is overloadable, collect all
                  --  interpretations of the name for subsequent overload
                  --  resolution. We optimize a bit here to do this only if we
                  --  have an overloadable entity that is not on its
                  --  own on the homonym chain.

               elsif Einfo.Is_Overloadable (E)
                 and then (Atree.Present (Einfo.Homonym (E)) or else
                           Sem_Util.Current_Entity (N) /= E)
               then
                  Sem_Type.Collect_Interps (N);

                  --  If no homonyms were visible, the entity is unambiguous

                  if not Sinfo.Is_Overloaded (N) then
                     if not Is_Actual_Parameter then
                        Lib.Xref.Generate_Reference (E, N);
                     end if;
                  end if;

                  --  Case of non-overloadable entity, set the entity
                  --  providing that we do not have the case of a discriminant
                  --  reference within a default expression. Such references
                  --  are replaced with the corresponding discriminal,
                  --  which is the formal corresponding to
                  --  to the discriminant in the initialization procedure.

               else
                  --  Entity is unambiguous, indicate that it is referenced
                  --  here

                  --  For a renaming of an object, always generate simple
                  --  reference, we don't try to keep track of assignments
                  --  in this case, except in SPARK mode where renamings are
                  --  traversed for generating local effects of subprograms.

                  if Einfo.Is_Object (E)
                    and then Atree.Present (Einfo.Renamed_Object (E))
                    and then not Opt.GNATprove_Mode
                  then
                     Lib.Xref.Generate_Reference (E, N);

                     --  If the renamed entity is a private protected
                     --  component, reference the original component as well.
                     --  This needs to be done because the private renamings
                     --  are installed before any analysis has occurred.
                     --  Reference to a private component will resolve to the
                     --  renaming and the original component will be
                     --  left unreferenced, hence the following.

                     if Einfo.Is_Prival (E) then
                        Lib.Xref.Generate_Reference (Einfo.Prival_Link (E), N);
                     end if;

                     --  One odd case is that we do not want to set the
                     --  Referenced flag if the entity is a label,
                     --  and the identifier is the label in the source,
                     --  since this is not a reference from the point of
                     --  view of the user.

                  elsif Atree.Nkind (Atree.Parent (N)) = Sinfo.N_Label then
                     declare
                        R : constant Boolean := Einfo.Referenced (E);

                     begin
                        --  Generate reference unless this is an actual
                        --  parameter (see comment below)

                        if Is_Actual_Parameter then
                           Lib.Xref.Generate_Reference (E, N);
                           Einfo.Set_Referenced (E, R);
                        end if;
                     end;

                     --  Normal case, not a label: generate reference

                  else
                     if not Is_Actual_Parameter then

                        --  Package or generic package is always a simple
                        --  reference

                        if Atree.Ekind (E) in
                          Einfo.E_Package | Einfo.E_Generic_Package
                        then
                           Lib.Xref.Generate_Reference (E, N, 'r');

                           --  Else see if we have a left hand side

                        else
                           case Sem_Util.Is_LHS (N) is
                              when Sem_Util.Yes =>
                                 Lib.Xref.Generate_Reference (E, N, 'm');

                              when Sem_Util.No =>
                                 Lib.Xref.Generate_Reference (E, N, 'r');

                                 --  If we don't know now, generate reference
                                 --  later

                              when Sem_Util.Unknown =>
                                 Lib.Xref.Deferred_References.Append ((E, N));
                           end case;
                        end if;
                     end if;
                  end if;

                  Set_Entity_Or_Discriminal (N, E);

                  --  The name may designate a generalized reference, in which
                  --  case the dereference interpretation will be included.
                  --  Context is one in which a name is legal.

                  if Opt.Ada_Version >= Opt.Ada_2012
                    and then
                      (Atree.Nkind (Atree.Parent (N)) in Sinfo.N_Subexpr
                       or else Atree.Nkind (Atree.Parent (N)) in
                         Sinfo.N_Assignment_Statement
                           | Sinfo.N_Object_Declaration
                             | Sinfo.N_Parameter_Association)
                  then
                     Sem_Util.Check_Implicit_Dereference (N, Sinfo.Etype (E));
                  end if;
               end if;
            end if;

            --  Come here with entity set

            Restrict.Check_Restriction_No_Use_Of_Entity (N);
            Result := E;
         else
            Result := Types.Empty;
         end if;
      end if;
      return Result;
   end Find_Entity_From_Direct_Name;

   function Find_Entity_From_Expanded_Name (N : Types.Node_Id)
                                          return Types.Entity_Id;

   function Find_Entity_From_Expanded_Name (N : Types.Node_Id)
                                            return Types.Entity_Id
   is
      pragma Unreferenced (N);
   begin
      return Types.Empty;
   end Find_Entity_From_Expanded_Name;

   ---------------------------
   -- Find_Entity_From_Name --
   ---------------------------

   function Find_Entity_From_Name (N : Types.Node_Id) return Types.Entity_Id
   is
      Result : Types.Entity_Id;
   begin
      if Atree.Nkind (N) = Sinfo.N_Identifier then
         Result := Find_Entity_From_Direct_Name (N);
      else
         Result := Find_Entity_From_Expanded_Name (N);
      end if;
      return Result;
   end Find_Entity_From_Name;

end SPARK_Classic.Pragmas.Atree_Utils;
