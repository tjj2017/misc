------------------------------------------------------------------------------
--                                                                          --
--                     SPARK_Classic_Examiner COMPONENTS                    --
--                                                                          --
--                                Tree_Walk                                 --
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
with Atree;             use Atree;
with Sinfo;             use Sinfo;
with Types;             use Types;

package Tree_Walk is

   type Examiner_Result_Type is (OK);

   procedure Do_Compilation_Unit (N : Node_Id;
                                  Result : out Examiner_Result_Type)
     with Pre => Nkind (N) = N_Compilation_Unit;

end Tree_Walk;
