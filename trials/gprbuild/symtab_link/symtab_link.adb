with Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Gnat.Strings;
use type Ada.Strings.Unbounded.Unbounded_String;
use type GNAT.OS_Lib.String_Access;

procedure SymTab_Link is
   No_Of_Args : constant Natural :=
     Ada.Command_Line.Argument_Count;
   Null_String : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.Null_Unbounded_String;
   Dir_String : Ada.Strings.Unbounded.Unbounded_String := Null_String;
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

   Text_IO.Put_Line ("The number of arguments is " & No_Of_Args'Image);
   loop
      case GNAT.Command_Line.Getopt ("d: o:") is
         when ASCII.NUL =>
            Text_IO.Put_Line ("NUL found");
            exit;
         when 'd' =>
            Text_IO.Put ("-d found: ");
            Text_IO.Put_Line (GNAT.Command_Line.Parameter);
            Dir_String := Ada.Strings.Unbounded.To_Unbounded_String
              (Ada.Strings.Fixed.Trim (GNAT.Command_Line.Parameter,
               Ada.Strings.Both));
         when 'o' =>
            Text_IO.Put ("-o found: ");
            Text_IO.Put_Line (GNAT.Command_Line.Parameter);
            Out_File_String := Ada.Strings.Unbounded.To_Unbounded_String
              (Ada.Strings.Fixed.Trim (GNAT.Command_Line.Parameter,
               Ada.Strings.Both));
         when others =>
            raise Program_Error;
      end case;
   end loop;

   declare
      File_Dir : constant String :=
        (if Dir_String /= Null_String then
            Ada.Strings.Fixed.Trim
           (Ada.Strings.Unbounded.To_String (Dir_String),
            Ada.Strings.Both) & "/"
         else
            "");

      Full_Out_File : constant String :=
        (if Out_File_String /= Null_String then
            " --out " & File_Dir &
            Ada.Strings.Unbounded.To_String (Out_File_String) & ".out"
         else
            "");

      Symtab_Files : GNAT.OS_Lib.String_Access :=
        new String'(File_Dir & "*.json_symtab");

      Out_File : GNAT.OS_Lib.String_Access :=
        new String'(Full_Out_File);

      Argument_List : GNAT.OS_Lib.Argument_List_Access :=
         new GNAT.OS_Lib.Argument_List'((Symtab_Files, Out_File));
   begin
      for I in Argument_List.all'Range loop
         Text_IO.Put_Line (Argument_List (I).all);
      end loop;

      Text_IO.Put_Line (Symtab2gb_Exe & " " & Symtab_Files.all & Out_File.all);
      GNAT.OS_Lib.Spawn (Symtab2gb_Exe, Argument_List.all, Success);
      GNAT.OS_Lib.Free (Argument_List);
   end;

   if Success then
      Text_IO.Put_Line ("Succsess");
   else
      Text_IO.Put_Line ("Failure");
   end if;

   Text_IO.Put_Line ("Argument: " & GNAT.Command_Line.Get_Argument);

end SymTab_Link;
