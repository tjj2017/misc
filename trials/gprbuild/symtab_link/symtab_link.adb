with Text_IO;
with Ada.Command_Line;
with GNAT.OS_Lib;
procedure SymTab_Link is
   No_Of_Args : constant Natural :=
     Ada.Command_Line.Argument_Count;
   Output_Dir : Natural := 0;
   Output_File : Natural := 0;
   Success : Boolean;
begin
   Text_IO.Put_Line ("The number of argumants is " & No_Of_Args'Image);
   for I in 1 .. No_Of_Args loop
      Text_IO.Put_Line (Ada.Command_Line.Argument (I));
      if Ada.Command_Line.Argument (I) = "-d" then
         Output_Dir := I + 1;
      elsif Ada.Command_Line.Argument (I) = "-o" then
         Output_File := I + 1;
      end if;
   end loop;
   Text_IO.Put_Line ("symtab2gb " &
                     (if Output_Dir /= 0 then
                Ada.Command_Line.Argument (Output_Dir) & "/*.json_symtab "
             else
                "*.json_symtab") &
             (if Output_File /= 0 then
                   "--out " & Ada.Command_Line.Argument (Output_File) & ".out"
                else
                           ""));
   GNAT.OS_Lib.Spawn ("symtab2gb",
                      ((if Output_Dir /= 0 then
                Ada.Command_Line.Argument (Output_Dir) & "/*.json_symtab "
             else
                "*.json_symtab"),
             (if Output_File /= 0 then
                   "--out " & Ada.Command_Line.Argument (Output_File) & ".out"
                else
                           "")),
                      Success);
end SymTab_Link;
