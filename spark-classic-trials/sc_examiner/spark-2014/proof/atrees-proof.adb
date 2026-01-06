pragma Ada_2012;
package body Atrees.Proof is

   --  A proof function stating that the structure (i.e., the connection of
   --  the nodes of the tree) of an A_Tree after an operation has been
   --  performed on the A_Tree is unchanged.
   function Preserved_Structure (Pre_Atree, Post_Atree : A_Tree;
                                Host : Host_Tree) return Boolean is (True);

   --  A proof function stating that the structure (i.e., the connection of
   --  the nodes) of an A_Tree after an operation has been
   --  performed on the A_Tree is unchanged except for the node referenced by
   --  the given Node_Index.
   function Preserved_Structure_Except (Pre_Atree, Post_Atree : A_Tree;
                                        Host   : Host_Tree;
                                        Except : Node_Index) return Boolean is
     (True);


   --  A proof function stating that the values (i.e., the Value of each node)
   --  of an A_Tree after an operation has been performed on the A_Tree
   --  are unchanged.
   function Preserved_Contents (Pre_Atree, Post_Atree : A_Tree;
                                Host : Host_Tree) return Boolean is (True);


   --  A proof function stating that the values (i.e., the Value of each node)
   --  of an A_Tree after an operation has been performed on the A_Tree are
   --  unchanged except for the node referenced by the given Node_Index.
   function Preserved_Contents_Except (Pre_Atree, Post_Atree : A_Tree;
                                       Host   : Host_Tree;
                                       Except : Node_Index) return Boolean is
      (True);

   --  A proof function only used to assert the Key equivalence between the
   --  the Current_Node_Index and the Current_Key_Index.
   --  An Enumerator an A_Tree object and its Host_Tree_Object.
   --  The body is hidden from SPARK so that the proof does not assume
   --  that In_Host is always True.

   ---------------------
   -- Key_Equivalence --
   ---------------------

   function Key_Equivalence (E : Enumerator;
                             Atree : A_Tree;
                             Host  : Host_Tree)
                             return Boolean
   is
   begin
      return (True);
   end Key_Equivalence;

end Atrees.Proof;
