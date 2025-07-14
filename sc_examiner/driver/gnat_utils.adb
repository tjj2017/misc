with Einfo;  use Einfo;
with Nlists; use Nlists;

package body GNAT_Utils is

   -----------------------
   -- Get_Called_Entity --
   -----------------------

   function Get_Called_Entity (N : Node_Id) return Entity_Id is
      Nam : constant Node_Id := Name (N);
   begin
      return
        (case Nkind (Nam) is
            when N_Selected_Component => Entity (Selector_Name (Nam)),
            when N_Indexed_Component => Entity (Selector_Name (Prefix (Nam))),
            when N_Explicit_Dereference => Etype (Nam),
            when others => Entity (Nam));
   end Get_Called_Entity;

   -----------------------------
   -- Iterate_Call_Parameters --
   -----------------------------

   procedure Iterate_Call_Parameters (Call : Node_Id)
   is
      Params     : constant List_Id := Parameter_Associations (Call);
      Cur_Formal : Entity_Id := First_Entity (Get_Called_Entity (Call));
      Cur_Actual : Node_Id := First (Params);
      In_Named   : Boolean := False;
   begin
      --  We have to deal with named arguments, but the frontend has
      --  done some work for us. All unnamed arguments come first and
      --  are given as-is, while named arguments are wrapped into a
      --  N_Parameter_Association. The field First_Named_Actual of the
      --  function or procedure call points to the first named argument,
      --  that should be inserted after the last unnamed one. Each
      --  Named Actual then points to a Next_Named_Actual. These
      --  pointers point directly to the actual, but Next_Named_Actual
      --  pointers are attached to the N_Parameter_Association, so to
      --  get the next actual from the current one, we need to follow
      --  the Parent pointer.
      --
      --  The Boolean In_Named states how to obtain the next actual:
      --  either follow the Next pointer, or the Next_Named_Actual of
      --  the parent.
      --  We start by updating the Cur_Actual and In_Named variables for
      --  the first parameter.

      if Nkind (Cur_Actual) = N_Parameter_Association then
         In_Named := True;
         Cur_Actual := First_Named_Actual (Call);
      end if;

      while Present (Cur_Formal) and then Present (Cur_Actual) loop
         Handle_Parameter (Cur_Formal, Cur_Actual);
         Cur_Formal := Next_Formal (Cur_Formal);

         if In_Named then
            Cur_Actual := Next_Named_Actual (Parent (Cur_Actual));
         else
            Next (Cur_Actual);

            if Nkind (Cur_Actual) = N_Parameter_Association then
               In_Named := True;
               Cur_Actual := First_Named_Actual (Call);
            end if;
         end if;
      end loop;
   end Iterate_Call_Parameters;

   -------------------------------
   -- Iterate_Pragma_Parameters --
   -------------------------------

   procedure Iterate_Pragma_Parameters (The_Pragma : Node_Id)
   is
      Params  : constant List_Id := Pragma_Argument_Associations (The_Pragma);
      pragma Assert (Is_Non_Empty_List (Params));

      Param   : Node_Id := First (Params);
      Arg_Pos : Positive := 1;

   begin
      Loop_Arg_Walk :
      while Present (Param) loop
         pragma Assert (Nkind (Param) = N_Pragma_Argument_Association);
         Handle_Arg (Arg_Pos  => Arg_Pos,
                     Arg_Name => Chars (Param),
                     Expr     => Expression (Param));

         Arg_Pos := Arg_Pos + 1;
         Next (Param);
      end loop Loop_Arg_Walk;
   end Iterate_Pragma_Parameters;

   function File_Name_Without_Suffix (File_Name : String) return String is
      Name_Index : Natural := File_Name'Last;

   begin
      pragma Assert (File_Name'Length > 0);

      --  We loop in reverse to ensure that file names that follow nonstandard
      --  naming conventions that include additional dots are handled properly,
      --  preserving dots in front of the main file suffix (for example,
      --  main.2.ada => main.2).

      while Name_Index >= File_Name'First
        and then File_Name (Name_Index) /= '.'
      loop
         Name_Index := Name_Index - 1;
      end loop;

      --  Return the part of the file name up to but not including the last dot
      --  in the name, or return the whole name as is if no dot character was
      --  found.

      if Name_Index >= File_Name'First then
         return File_Name (File_Name'First .. Name_Index - 1);

      else
         return File_Name;
      end if;
   end File_Name_Without_Suffix;

end GNAT_Utils;
