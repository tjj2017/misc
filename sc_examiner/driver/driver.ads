------------------------------------------------------------------------------
--                                                                          --
--                     SPARK_Classic_Examiner COMPONENTS                    --
--                                                                          --
--                               D R I V E R                                --
--                                                                          --
--                                  SPEC                                    --
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

with Types; use Types;

package Driver is
   procedure SPARK_Classic_Examine (GNAT_Root : Node_Id);
   procedure Examine_Compilation_Unit (GNAT_Root : Node_Id);

   function Is_Back_End_Switch (Switch : String) return Boolean;

end Driver;
