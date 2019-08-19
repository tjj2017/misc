with Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Directory_Operations;
use type Ada.Strings.Unbounded.Unbounded_String;
use type GNAT.OS_Lib.String_Access;

procedure SymTab_Link is
   --  gprbuild looks for linker input files in the directory specified
   --  by Object_Dir in the gpr file
   Input_Files : constant String := "*.json_symtab";

   No_Of_Args : constant Natural :=
     Ada.Command_Line.Argument_Count;
   Null_String : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.Null_Unbounded_String;
   Space : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String (" ");
   Out_File_String : Ada.Strings.Unbounded.Unbounded_String := Null_String;

   Symtab2gb_Exe_Access : constant GNAT.OS_Lib.String_Access :=
     GNAT.OS_Lib.Locate_Exec_On_Path ("symtab2gb");
   Symtab2gb_Exe : constant String :=
     (if Symtab2gb_Exe_Access /= null then
         Symtab2gb_Exe_Access.all
      else
         "");

   Success : Boolean := False;
begin
   if Symtab2gb_Exe = "" then
      Text_IO.Put_Line ("symtab2gb executable not found on path");
      return;
   end if;

--   Text_IO.Put_Line ("The number of arguments is " & No_Of_Args'Image);
   loop
      case GNAT.Command_Line.Getopt ("o:") is
         when ASCII.NUL =>
--            Text_IO.Put_Line ("NUL found");
            exit;
         when 'o' =>
--            Text_IO.Put ("-o found: ");
--            Text_IO.Put_Line (GNAT.Command_Line.Parameter);
            Out_File_String := Ada.Strings.Unbounded.To_Unbounded_String
              (Ada.Strings.Fixed.Trim (GNAT.Command_Line.Parameter,
               Ada.Strings.Both));
         when others =>
            raise Program_Error;
      end case;
   end loop;

   declare
      Out_File : constant String :=
        Ada.Strings.Unbounded.To_String (Out_File_String);

      Full_Out_File : constant String :=
        (if Out_File /= "" then
            " --out " &
         (if GNAT.Directory_Operations.File_Extension (Out_File) = "" then
               Out_File & ".out"
            else
               Out_file)
         else
            "");

      File_Iter : GNAT.Command_Line.Expansion_Iterator;
      Arg_String : Ada.Strings.Unbounded.Unbounded_String := Null_String;

   begin

      GNAT.Command_Line.Start_Expansion
        (Iterator => File_Iter,
         Pattern => Input_Files,
         Directory => "",
         Basic_Regexp => True);

      loop
         declare Next_File : constant String :=
              GNAT.Command_Line.Expansion (File_Iter);
         begin
            exit when Next_File = "";
            Arg_String := Arg_String & Space &
              Ada.Strings.Unbounded.To_Unbounded_String (Next_File);
         end;
      end loop;

      declare
         Fixed_Arg_String : constant String :=
           Ada.Strings.Fixed.Trim
             (Ada.Strings.Unbounded.To_String (Arg_String),
              Ada.Strings.Both);

         Argument_List : GNAT.OS_Lib.Argument_List_Access;
      begin

--         Text_IO.Put_Line ("Expanded files: " &
--                             Fixed_Arg_String & Full_Out_File);
         if Fixed_Arg_String /= "" then
            Argument_List := GNAT.OS_Lib.Argument_String_To_List
              (Fixed_Arg_String & Full_Out_File);
         else
            Text_IO.Put_Line ("At least one json_symtab file must be given");
         end if;

         -- GNAT.OS_LIB.Normalize_Arguments (Argument_List.all);
--         for I in Argument_List.all'Range loop
--            Text_IO.Put_Line (Argument_List (I).all);
--         end loop;

         Text_IO.Put_Line (Symtab2gb_Exe & " " & Fixed_Arg_String);
         GNAT.OS_Lib.Spawn (Symtab2gb_Exe, Argument_List.all, Success);
         GNAT.OS_Lib.Free (Argument_List);
      end;

   end;

   if Success then
      Text_IO.Put_Line ("Succsess");
   else
      Text_IO.Put_Line ("Failure");
   end if;

   Text_IO.Put_Line ("Argument: " & GNAT.Command_Line.Get_Argument);

end SymTab_Link;
