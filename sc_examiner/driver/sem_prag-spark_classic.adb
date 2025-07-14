with Sinfo;
with Sem_Util;
with Treepr; use Treepr;
with SPARK_Classic;
with SPARK_Classic.Pragmas;
with Ada.Text_IO; use Ada.Text_IO;
package body Sem_Prag.SPARK_Classic is

   --------------------
   -- Process_Pragma --
   --------------------

   procedure Process_Pragma (N : Types.Node_Id; Arg : Types.Node_Id)
   is
      Pname     : constant Namet.Name_Id := Sinfo.Pragma_Name (N);
      Prag_Id   : constant Snames.Pragma_Id := Snames.Get_Pragma_Id (Pname);
      Prag_Name : constant Namet.Name_Id :=
        Sem_Util.Original_Aspect_Pragma_Name (N);
      Prag_Context : constant Standard.SPARK_Classic.Ada_Contexts :=
        Standard.SPARK_Classic.Context_Of_Parent (N);

      Pragma_Error    : Standard.SPARK_Classic.Pragmas.Pragma_Errors;
      Applies_To      : Types.Node_Id;
      Applies_Context : Standard.SPARK_Classic.Contexts;
      Applies_Entity  : Types.Node_Id;
   begin
      Put_Line ("SPARK Classic Pragma " &
                  Namet.Get_Name_String (Pname) & ":" &
               Namet.Get_Name_String (Prag_Name));
      Print_Node_Briefly (N);
      Print_Node_Briefly (Arg);
      Put_Line ("Prag_Id " & Snames.Pragma_Id'Image (Prag_Id));
      Put_Line ("Checking pragma placement");
      Standard.SPARK_Classic.Pragmas.Placement_Defns.Check_Placement
        (N              => N,
         Classic_Anno   => SPARK_Classic.Context_Of_Node (N),
         Parent_Context => Prag_Context,
         Applies_To     => Applies_To,
         Applies_Ctx    => Applies_Context,
         Applies_Entity => Applies_Entity,
         Pragma_Error   => Pragma_Error);
      if Pragma_Error = SPARK_Classic.Pragmas.None then
         Put_Line ("No error detected");
         Print_Node_Briefly (Applies_To);
         Print_Node_Briefly (Applies_Entity);
         Put_Line (SPARK_Classic.Contexts'Image (Applies_Context));
      else
         Put_Line ("Error: " &
                     SPARK_Classic.Pragmas.Pragma_Errors'Image (Pragma_Error));
      end if;
   end Process_Pragma;

end Sem_Prag.SPARK_Classic;
