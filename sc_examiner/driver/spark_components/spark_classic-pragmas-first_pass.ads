with Types;
package SPARK_Classic.Pragmas.First_Pass is
   --  As a rule the new Examiner acts as a back-end to the Gnat front-end.
   --  However there are a number of situations where some processing has to
   --  be added to the front-end when semantically analyzing SPARK Classic
   --  aspects/pragmas.  These situations are handled by the subprograms in
   --  this package.

   procedure Analyze_Exprns (N : Types.Node_Id; Arg : Types.Node_Id);
   --  Scan the pragma argument for expressions that need to be semantically
   --  analysed during semantic analysis by the front-end
   --  (called by the front-end).

end SPARK_Classic.Pragmas.First_Pass;
