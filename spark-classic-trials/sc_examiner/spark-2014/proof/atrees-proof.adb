pragma Ada_2012;
package body Atrees.Proof is

   function Enumerator_Of_Tree (E : Enumerator;
                                A : A_Tree;
                                T : Host_Tree)
                                return Boolean with
     SPARK_Mode => Off
   is
   begin
      return (True);
   end Enumerator_Of_Tree;

  ---------------------
   -- Key_Equivalence --
   ---------------------

   function Key_Equivalence (E : Enumerator;
                             A : A_Tree;
                             T : Host_Tree)
                             return Boolean with
     SPARK_Mode => Off
   is
   begin
      return (True);
   end Key_Equivalence;

end Atrees.Proof;
