with Namet, Snames, Nlists, Sinfo, Sem_Res;
use type Snames.Pragma_Id;

with Text_IO; use Text_IO;
with Treepr;  use Treepr;
package body SPARK_Classic.Pragmas.First_Pass is

   procedure Analyze_Property_Exprns (Own_Obj_Spec : Types.Node_Id);

   procedure Analyze_Property_Exprns (Own_Obj_Spec : Types.Node_Id) is
      Prop : Types.Node_Id := Nlists.First
        (Sinfo.Component_Associations (Own_Obj_Spec));
   begin
      Put_Line ("Analyzing property expressions");
      Print_Node_Briefly (Own_Obj_Spec);
      while Atree.Present (Prop) loop
         declare
            Own_Modifier : constant Types.Node_Id :=
              Nlists.First (Sinfo.Choices (Prop));
         begin
            Print_Node_Briefly (Prop);
            Print_Node_Briefly (Own_Modifier);
            if Atree.Present (Own_Modifier) and then
              Atree.Nkind (Own_Modifier) = Sinfo.N_Identifier
            then
               if Has_Chars (Own_Modifier, "integrity") then
                  Put_Line ("Integrity expression");
                  Sem_Res.Analyze_And_Resolve (Sinfo.Expression (Prop));
                  Print_Node_Briefly (Sinfo.Expression (Prop));
               elsif Has_Chars (Own_Modifier, "priority") then
                  Put_Line ("Priority expression");
                  Sem_Res.Analyze_And_Resolve (Sinfo.Expression (Prop));
                  Print_Node_Briefly (Sinfo.Expression (Prop));
               end if;
            end if;
            Prop := Nlists.Next (Prop);
         end;
      end loop;
   end Analyze_Property_Exprns;

   --------------------
   -- Analyze_Exprns --
   --------------------

   procedure Analyze_Exprns (N : Types.Node_Id; Arg : Types.Node_Id) is
      Pname     : constant Namet.Name_Id := Sinfo.Pragma_Name (N);
      Prag_Id   : constant Snames.Pragma_Id := Snames.Get_Pragma_Id (Pname);
--        Prag_Name : constant Namet.Name_Id :=
--          Sem_Util.Original_Aspect_Pragma_Name (N);
   begin
      --  At the moment only the value expressions of Integrity and Priority
      --  are need to be analysed during front-end semantic analysis.
      --  If this is not done during front-end semantic analysis, evaluation
      --  of the values may give incorrect results (may use a local object
      --  than one specified by an extended name) or may fail because
      --  a with'd package is not taken to be needed.
      if Prag_Id = Snames.Pragma_Classic_Own then
         Put_Line ("First pass Classic_Own");
         Print_Node_Briefly (Arg);
         if Atree.Nkind (Arg) = Sinfo.N_Extension_Aggregate then
            Analyze_Property_Exprns (Arg);
         elsif Atree.Nkind (Arg) = Sinfo.N_Aggregate then
            declare
               Own_Obj_Spec : Types.Node_Id :=
                 Nlists.First (Sinfo.Expressions (Arg));
            begin
               while Atree.Present (Own_Obj_Spec) loop
                  if Atree.Nkind (Own_Obj_Spec) = Sinfo.N_Extension_Aggregate
                  then
                     Analyze_Property_Exprns (Own_Obj_Spec);
                  end if;
                  Own_Obj_Spec := Nlists.Next (Own_Obj_Spec);
               end loop;
            end;
         end if;
      end if;
   end Analyze_Exprns;

end SPARK_Classic.Pragmas.First_Pass;
