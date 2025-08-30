with Nlists;
package body SPARK_Classic.Pragmas.Syntax is

   ------------------------
   -- Get_Aggregate_Sort --
   ------------------------

   function Get_Aggregate_Sort (Aggregate : Types.Node_Id)
                                return Aggregate_Sort is
      Result : Aggregate_Sort;
   begin
      if Atree.Nkind (Aggregate) = Sinfo.N_Extension_Aggregate then
         Result := Extension;
      elsif Nlists.Present (Sinfo.Component_Associations (Aggregate)) then
         Result := Comp_Assocs;
      elsif Nlists.Present (Sinfo.Expressions (Aggregate)) then
         Result := Exprs;
      else
         Result := Unknown;
      end if;
      return Result;
   end Get_Aggregate_Sort;

end SPARK_Classic.Pragmas.Syntax;
