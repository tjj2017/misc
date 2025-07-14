package body SPARK_Classic.Pragmas.Context_Defns is

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context
     (N            : Types.Node_Id;
      Context      : out SPARK_Classic.Pragmas.Contexts;
      Pragma_Error : out SPARK_Classic.Pragmas.Pragma_Errors)
   is
      SPARK_Anno    : constant
        SPARK_Classic.Pragmas.SPARK_Classic_Annotations :=
          Name_To_Anno (Sinfo.Pragma_Name (N));
      Context_Rstrct : Restriction;
      Found          : Boolean := False;
      Done           : Boolean := False;
   begin
      Pragma_Error := SPARK_Classic.Pragmas.None;
      Context := Get_Context (N);

      for I in Index loop
         Context_Rstrct := Rules (SPARK_Anno) (I);
         case Context_Rstrct.Perm is
            when SPARK_Classic.Pragmas.Only_This =>
               --  There can only be one enclosing declaration context
               --  If the permission is "Only_This" it is either in the
               --  right context or not.  No need to look any further.
               --  If the context is right, a suitable context has been found.
               Done := True;
               if Context = Context_Rstrct.Ctx then
                  Found := True;
               else
                  Pragma_Error := SPARK_Classic.Pragmas.Wrong_Only_Context;
               end if;
            when SPARK_Classic.Pragmas.Not_This =>
               --  If the declaration context matches a context with
               --  a permission "Not_This" it is an error. Terminate loop.
               Done := True;
               Pragma_Error := SPARK_Classic.Pragmas.Invalid_Context;
            when SPARK_Classic.Pragmas.Only_One =>
               --  The annotation can only be in one declaration context
               --  but several different ones may be valid.
               --  If the declaration context matches the permitted context
               --  then a suitable context has been found and can be the only
               --  one and the loop can be terminated.
               --  Otherwise continue looking through the permitted contexts.
               if Context = Context_Rstrct.Ctx then
                  Found := True;
                  Done := True;
               end if;
            when SPARK_Classic.Pragmas.Nothing =>
               --  The list of permitted contexts is completed -
               --  Terminate the loop.
               Done := True;
            when others =>
               --  The other permissions are not used in context
               --  restrictions and are used in placement restrictions.
               --  Ignore.
               null;
         end case;
         exit when Done;
      end loop;

      --  Has a suitable context been found?
      if not Found then
         Pragma_Error := SPARK_Classic.Pragmas.Unsuitable_Context;
      end if;

   end Check_Context;

end SPARK_Classic.Pragmas.Context_Defns;
