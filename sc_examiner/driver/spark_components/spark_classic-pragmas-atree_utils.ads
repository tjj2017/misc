with Types, Atree, Sinfo;
use type Types.Node_Id, Types.Int;
package SPARK_Classic.Pragmas.Atree_Utils is
   function Find_Entity_From_Name (N : Types.Node_Id) return Types.Entity_Id
     with Pre => Atree.Nkind (N) in Sinfo.N_Identifier |
                 Sinfo.N_Selected_Component;
   --  Given an identifier or, a selected component representing an
   --  expanded name, find the entity, if any, associated with the name.
   --  Returns the entity if found, otherwise returns Empty.

end SPARK_Classic.Pragmas.Atree_Utils;
