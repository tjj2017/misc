with Types, Atree, Sinfo;
--  # acquire SPARK_Classic, SPARK_Classic.Pragmas, Nlists, Einfo, Sem_Util;
package SPARK_Classic.Pragmas.Syntax is
   type Aggregate_Sort is (Comp_Assocs, Exprs, Extension, Unknown);

   function Get_Aggregate_Sort (Aggregate : Types.Node_Id)
                                return Aggregate_Sort
   with Pre => Atree.Nkind (Aggregate) = Sinfo.N_Aggregate;

   type Modifier

end SPARK_Classic.Pragmas.Syntax;
