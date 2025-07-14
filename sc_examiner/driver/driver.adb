------------------------------------------------------------------------------
--                                                                          --
--                     SPARK_Classic_Examiner COMPONENTS                    --
--                                                                          --
--                               D R I V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2025, Trevor Jennings                   --
--                                                                          --
-- SPARK_Classic_Examiner is free software;                                 --
-- you can redistribute it and/or modify it under terms of the              --
-- GNU General Public License as published by the Free Software  Foundation;--
-- either version 3, or (at your option)  any later version.                --
-- The SPARK_Classic_Examiner is distributed in the hope that it is useful, --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public License  distributed with gnat2goto;  see file COPYING3. --
-- If not,  go to  http://www.gnu.org/licenses  for a complete  copy of the --
-- license.                                                                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;

with Switch;                use Switch;

with Atree;                 use Atree;
with Tree_Walk;             use Tree_Walk;

with Sinfo;                 use Sinfo;

with SPARK_Classic_Examiner.Options;

package body Driver is
   procedure SPARK_Classic_Examine (GNAT_Root : Node_Id)
   is
   begin
      --  Initialization of SPARK Examiner components required here.
      Examine_Compilation_Unit (GNAT_Root);
   end SPARK_Classic_Examine;

   procedure Examine_Compilation_Unit (GNAT_Root : Node_Id)
   is
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);
      Result : Examiner_Result_Type;

   begin
      Do_Compilation_Unit (GNAT_Root, Result);
      case Result is
         when others =>
            Put_Line ("Examination complete");
      end case;

   end Examine_Compilation_Unit;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Natural := Switch'First + 1;
      Last  : constant Natural := Switch_Last (Switch);
      use SPARK_Classic_Examiner.Options;
   begin
      --  For now we allow the -g/-O/-f/-m/-W/-w and -pipe switches, even
      --  though they will have no effect. This permits compatibility with
      --  existing scripts.
      if Is_Switch (Switch) then
         if Switch (First) in 'f' | 'g' | 'm' | 'O' | 'W' | 'w'
           or else Switch (First .. Last) = "pipe"
         then
            return True;
         elsif Switch (First .. Last) = Dump_Statement_AST_On_Error_Option
         then
            Dump_Statement_AST_On_Error := True;
            return True;
         end if;
      end if;
      return False;
   end Is_Back_End_Switch;

end Driver;
