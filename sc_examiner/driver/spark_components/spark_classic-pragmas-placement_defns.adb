with Einfo, Nlists, Sem_Util;
package body SPARK_Classic.Pragmas.Placement_Defns is

   procedure Apply_Placement_Rules
     (Default_Context    : SPARK_Classic.Contexts;
      Check_Item         : Types.Node_Id;
      Placements         : Placement_List;
      Stop_On_Sufficient : Boolean;
      Validating_Node    : out Types.Node_Id;
      Pragma_Error       : out Pragma_Errors);

   procedure Apply_Placement_Rules
     (Default_Context    : SPARK_Classic.Contexts;
      Check_Item         : Types.Node_Id;
      Placements         : Placement_List;
      Stop_On_Sufficient : Boolean;
      Validating_Node    : out Types.Node_Id;
      Pragma_Error       : out Pragma_Errors)
   is
      Curr_Item        : Types.Node_Id := Check_Item;
      Curr_Context     : SPARK_Classic.Contexts;
      Scan_Prior_Items : Boolean;
   begin
      --  Assume that there will be no errors
      Pragma_Error := SPARK_Classic.Pragmas.None;
      --  but a validating node has not been found.
      Validating_Node := Types.Empty;

      --  The checking of the placment of an item (an aspect definition or
      --  a pragma) starts from the given Check_Item back up the list of items.
      --  The context of each item is determined and the scaning of previous
      --  items is terminated when a validating context is found, a context
      --  is invalid or there are no previous items
      --  If there no previous Items, then the Default_Context is used before
      --  exiting from the loop.
      if Nlists.Is_List_Member (Curr_Item) and then
        Atree.Present (Curr_Item) and then
        Atree.Present (Nlists.Prev (Curr_Item))
      then
         --  There is a previous item, make it the current item in the
         --  list and obtain its context.
         Curr_Item := Nlists.Prev (Curr_Item);
         Curr_Context :=
           SPARK_Classic.Context_Of_Node (Curr_Item);
         --  There is at least one previous item.
         Scan_Prior_Items := True;
      else
         --  The the given item is the first or only item in the list.
         --  Make the Default_Context the current context.
         --  There are no prior items.
         Curr_Item := Check_Item;
         Curr_Context := Default_Context;
         Scan_Prior_Items := False;
      end if;

      --  Check the current and previous contexts.
      loop
         --  A SPARK_Classic pragma may be preceded by pragmas which arise
         --  from aspect specifications or possibly the front-end and have
         --  to be ignored.
         --  If the current context is a pragma arising from an aspect
         --  specification or is not from the source code skip it.
         --  Otherwise check the validity of this item.
         if Atree.Comes_From_Source (Curr_Item) and then
           not (Atree.Nkind (Curr_Item) = Sinfo.N_Pragma and then
                Sinfo.From_Aspect_Specification (Curr_Item))
         then
            --  Check that this context is allowed.
            case Placements (Curr_Context) is
            when This_Must =>
               --  A mandatory validating context has been reached, any
               --  intervining items have been processed and are permitted.
               --  No further checking or scanning prior aspects required.
               Validating_Node := Curr_Item;
               Scan_Prior_Items := False;
            when Only_One =>
               if Curr_Context in SPARK_Classic.Ada_Contexts then
                  --  Where one of a number of Ada contexts is
                  --  permitted, only one can actully be present
                  --  and it is the required validating context, any
                  --  intervining items have been processed and are permitted.
                  --  No further checking required.
                  Validating_Node := Curr_Item;
                  Scan_Prior_Items := False;
               else
                  --  One of a number of previous contexts are required but
                  --  only one.
                  if Atree.Present (Validating_Node) then
                     --  One of the options has been processed and another
                     --  has been encountered - this is an error.
                     Pragma_Error :=
                       SPARK_Classic.Pragmas.More_Than_One_Context;
                     Scan_Prior_Items := False;
                  else
                     --  Continue checking previous aspects but
                     --  note that one candidate has been found and is
                     --  correct if no other candidates are found.
                     Validating_Node := Curr_Item;
                  end if;
               end if;
            when Suffices =>
               if Stop_On_Sufficient then
                  --  The current context is sufficient to validate the
                  --  placement of the annotation/ pragma.
                  --  No more scanning required.
                  Validating_Node := Curr_Item;
                  Scan_Prior_Items := False;
               else
                  --  Treat as This_May
                  null;
               end if;
            when This_May =>
               --  This item is permitted, continue checking.
               null;
            when Anything =>
               --  No restrictions.  No further checking required.
               --  Use the default context as the validating context.
               Validating_Node := Check_Item;
               Scan_Prior_Items := False;
            when Not_Allowed =>
               --  This is an error - don't check any further.
               Scan_Prior_Items := False;
            end case;
         end if;
         exit when not Scan_Prior_Items;

         if Atree.Present (Nlists.Prev (Curr_Item)) then
            Curr_Item := Nlists.Prev (Curr_Item);
            Curr_Context :=
              SPARK_Classic.Context_Of_Node (Curr_Item);
         else
            --  There are no more previous items use the default context.
            Curr_Context := Default_Context;
            --  There are no prior aspect definitions.
            Scan_Prior_Items := False;
         end if;
      end loop;

      --  If a validating node has not been found the annotation/pragma
      --  is misplaced.
      if not Atree.Present (Validating_Node) then
         Pragma_Error := SPARK_Classic.Pragmas.Misplaced_Annotation;
      end if;
   end Apply_Placement_Rules;

   --  The placement rules are interpreted differently depending on
   --  whether the pragma comes from an aspect specification.
   procedure Check_Placement_From_Aspect
     (N              : Types.Node_Id;
      Allowed_Here   : Placement_List;
      Applies_To     : out Types.Node_Id;
      Applies_Ctx    : out SPARK_Classic.Ada_Contexts;
      Applies_Entity : out Types.Entity_Id;
      Pragma_Error   : out SPARK_Classic.Pragmas.Pragma_Errors);

   procedure Check_Placement_From_Aspect
     (N              : Types.Node_Id;
      Allowed_Here   : Placement_List;
      Applies_To     : out Types.Node_Id;
      Applies_Ctx    : out SPARK_Classic.Ada_Contexts;
      Applies_Entity : out Types.Entity_Id;
      Pragma_Error   : out SPARK_Classic.Pragmas.Pragma_Errors)
   is
      --  Get the definition of the aspect representing the SPARK  Classic
      --  annotation to be checked;
      Check_Aspect       : constant Types.Node_Id :=
        Sinfo.Corresponding_Aspect (N);
      Validating_Node    : Types.Node_Id;
   begin
      --  Of all the SPARK annotations, only those which are
      --  Entity_Annotations may be represented by aspects.
      --  Such aspects will apply to a declared Ada entity.

      --  Obtain the entity to which the checked aspect applies.
      --  It should be present.
      Applies_Entity := Sinfo.Entity (Check_Aspect);
      if Atree.Present (Applies_Entity) then
         --  From the entity obtain its declaration.
         --  Again, it should be present.
         Applies_To := Einfo.Declaration_Node (Applies_Entity);
         --  Obtain the SPARK Classic context - a Blank_Context will be
         --  returned if Appies_Entity is not present.
         Applies_Ctx := SPARK_Classic.Context_Of_Node (Applies_To);
      else
         --  This should not happen.
         Applies_To := Types.Empty;
         Applies_Ctx := SPARK_Classic.Blank_Context;
      end if;

      --  Aspects representing SPARK annotations are placed prior to
      --  any other aspect applying to the entity.
      --  The checking works from the aspect representing the annotation
      --  (the Check_Aspect) back up the list of aspect definitions.
      --  The context of the declaration to which the aspect appies is the
      --  Default_Context which will be used if there are no previous aspect
      --  definitions.
      Apply_Placement_Rules
        (Default_Context    => Applies_Ctx,
         Check_Item         => Check_Aspect,
         Placements         => Allowed_Here,
         Stop_On_Sufficient => True,
         Validating_Node    => Validating_Node,
         Pragma_Error       => Pragma_Error);

      pragma Assert (Atree.Present (Validating_Node) or
                       Pragma_Error /= SPARK_Classic.Pragmas.None);
   end Check_Placement_From_Aspect;

   procedure Check_Pragma_Placement
     (Check_Pragma   : Types.Node_Id;
      Parent_Context : SPARK_Classic.Contexts;
      Allowed_Here   : Placement_List;
      Applies_To     : out Types.Node_Id;
      Applies_Ctx    : out SPARK_Classic.Ada_Contexts;
      Applies_Entity : out Types.Entity_Id;
      Pragma_Error   : out SPARK_Classic.Pragmas.Pragma_Errors);

   procedure Check_Pragma_Placement
     (Check_Pragma   : Types.Node_Id;
      Parent_Context : SPARK_Classic.Contexts;
      Allowed_Here   : Placement_List;
      Applies_To     : out Types.Node_Id;
      Applies_Ctx    : out SPARK_Classic.Ada_Contexts;
      Applies_Entity : out Types.Entity_Id;
      Pragma_Error   : out SPARK_Classic.Pragmas.Pragma_Errors)
   is
      Pragma_Context           : constant SPARK_Classic.Contexts :=
        SPARK_Classic.Context_Of_Node (Check_Pragma);
      Pragma_Applies_To_Entity : constant Boolean :=
        Pragma_Context in SPARK_Classic.Entity_Annotations;
      Validating_Node          : Types.Node_Id;
      Validating_Context       : SPARK_Classic.Contexts;
   begin
      --  SPARK Classic pragmas representing Entity_Annotations are placed
      --  after the entity to which they apply.  However, there may be other
      --  pragmas arising from an aspect specification that may appear in the
      --  Atree between the pragma being checked and the entity to which it
      --  applies.  If an intervening pragma from an aspect is a SPARK Classic
      --  pragma its validity is determined according to the rules specified
      --  for the given (by parameter N) SPARK Classic pragma.
      --  There should not be any intervining items, other than pragmas
      --  between the given pragma and the entity to which it applies.

      --  The placement is validated by checking the SPARK_Classic.Context of
      --  each item prior the given pragma from the list of items of the
      --  Parent_Context.
      --  If there no previous items in the list, then the Parent_Context is
      --  taken as the default context.
      Apply_Placement_Rules
        (Default_Context    =>
           SPARK_Classic.Context_Of_Node (Atree.Parent (Check_Pragma)),
         Check_Item         => Check_Pragma,
         Placements         => Allowed_Here,
         Stop_On_Sufficient => False,
         Validating_Node    => Validating_Node,
         Pragma_Error       => Pragma_Error);

      if Pragma_Error = SPARK_Classic.Pragmas.None then
         if Pragma_Applies_To_Entity then
            --  Obtain the entity from the qualifying node
            Validating_Context :=
              SPARK_Classic.Context_Of_Node (Validating_Node);
            if Validating_Context not in SPARK_Classic.Ada_Contexts then
               Validating_Node := Atree.Parent (Validating_Node);
               Validating_Context :=
                 SPARK_Classic.Context_Of_Node (Validating_Node);
            end if;

            if Validating_Context in SPARK_Classic.Ada_Contexts then
               Applies_To := Validating_Node;
               Applies_Ctx := Validating_Context;
               Applies_Entity := Sem_Util.Defining_Entity
                 (N               => Validating_Node,
                  Empty_On_Errors => True);
               if not Atree.Present (Applies_Entity) then
                  Pragma_Error := SPARK_Classic.Pragmas.Misplaced_Annotation;
               end if;
            else
               Pragma_Error := SPARK_Classic.Pragmas.Misplaced_Annotation;
               Applies_Ctx := Parent_Context;
            end if;
         end if;
      else
         Applies_Ctx := Parent_Context;
      end if;
   end Check_Pragma_Placement;

   ---------------------
   -- Check_Placement --
   ---------------------

   procedure Check_Placement
     (N              : Types.Node_Id;
      Classic_Anno   : SPARK_Classic.SPARK_Classic_Annotations;
      Parent_Context : SPARK_Classic.Ada_Contexts;
      Applies_To     : out Types.Node_Id;
      Applies_Ctx    : out SPARK_Classic.Ada_Contexts;
      Applies_Entity : out Types.Entity_Id;
      Pragma_Error   : out SPARK_Classic.Pragmas.Pragma_Errors)
   is
   begin
      if Parent_Context /= SPARK_Classic.Blank_Context and
        Classic_Anno /= SPARK_Classic.Classic_Blank
      then
         if Sinfo.From_Aspect_Specification (N) then
            Check_Placement_From_Aspect
              (N              => N,
               Allowed_Here   => Rules (Classic_Anno) (Parent_Context),
               Applies_To     => Applies_To,
               Applies_Ctx    => Applies_Ctx,
               Applies_Entity => Applies_Entity,
               Pragma_Error   => Pragma_Error);
         else
            Check_Pragma_Placement
              (Check_Pragma   => N,
               Parent_Context => Parent_Context,
               Allowed_Here   => Rules (Classic_Anno) (Parent_Context),
               Applies_To     => Applies_To,
               Applies_Ctx    => Applies_Ctx,
               Applies_Entity => Applies_Entity,
               Pragma_Error   => Pragma_Error);
         end if;
      else
         Pragma_Error := SPARK_Classic.Pragmas.Invalid_Context;
         Applies_To := Types.Empty;
         Applies_Ctx := SPARK_Classic.Blank_Context;
         Applies_Entity := Types.Empty;
      end if;
   end Check_Placement;

end SPARK_Classic.Pragmas.Placement_Defns;
