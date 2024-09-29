--------------------------  SPARK_2014.Tree_Abs  -----------------------------
--  This package is a stateless abstraction providing basic operations on   --
--  an underlying tree.  The tree is represented by its collection of       --
--  Tree_Nodes, which are also the embodiment of the state hidden within    --
--  the package body. The body of this package must be excluded from SPARK  --
--  analysis because of the hidden state.                                   --                                                          --
--  The collection of all Tree_Nodes represents a single tree.  There is    --
--  no type to enable multiple tree decarations.  If multiple trees are     --
--  required then, either multiple instatnces of this package are required  --
--  or multiple sub-trees have to be accommodated within the single tree    --
--  structure this package provides.                                        --
--  Each node of the Tree has 5 fields:                                     --
--  Left, Right : TT_Nree_Node;   --  the children of the node                 --
--  Level       : Level_Type;  --  the level of the node in the Tree        --
--  Key         : Key_Type;    --  place for key of node                    --
--  Key needs to have a null value provided.                  --
--  The values of these fields are set and interrogated by the subprograms  --
--  declared below.                                                         --
--  Procedure Init should be called prior to using the tree structure.      --
------------------------------------------------------------------------------
--# inherit SPARK_2005.Multi_ATree;
private package SPARK_2005.Multi_ATree.Tree_Abstraction is
   subtype Level_Type is Natural;

   procedure Init;

   function Is_A_Valid_Tree_Node (N : Multi_ATree.Tree_Node) return Boolean;

   function Is_Empty (N :  Multi_ATree.Tree_Node) return Boolean;

   function In_Tree  (N :  Multi_ATree.Tree_Node) return Boolean;
   --# return IT => IT -> (not Is_Empty (N) and Is_A_Valid_Tree_Node (N) and
   --#                     N in Multi_ATree.Valid_Tree_Node);

   function Level (N : Multi_ATree.Valid_Tree_Node) return Level_Type;

   function Left  (N : Multi_ATree.Valid_Tree_Node)
                   return  Multi_ATree.Tree_Node;
   --# return L => (not Is_Empty (L)) -> In_Tree (L);

   function Right (N : Multi_ATree.Valid_Tree_Node)
                   return  Multi_ATree.Tree_Node;
   --# return R => (not Is_Empty (R)) -> In_Tree (R);

   function Key (N : Multi_ATree.Valid_Tree_Node) return Multi_ATree.Key_Type;

   -- In the following procedure declarations the Tree_Node parameter, N,
   -- has been designated as mode in out, even though N is not modified
   -- by calling them.  This is how the underlying state hidden in the
   -- package body is represented in this abstraction.
   procedure Set_Level (N : in out Multi_ATree.Valid_Tree_Node;
                        Node_Level : Level_Type);
   --# post Level (N) = Node_Level and In_Tree (N);

   procedure Set_Left  (N : in out Multi_ATree.Valid_Tree_Node;
                        Branch :  Multi_ATree.Tree_Node);
   --# post  Left (N) = Branch and In_Tree (N);

   procedure Set_Right (N : in out Multi_ATree.Valid_Tree_Node;
                        Branch :  Multi_ATree.Tree_Node);
   --# post Right (N) = Branch and In_Tree (N);

   procedure Set_Key (N : in out Multi_ATree.Valid_Tree_Node;
                      The_Key : Multi_ATree.Key_Type);
   --# post Key (N) = The_Key and In_Tree (N);

   procedure Add_Node  (N : out Multi_ATree.Valid_Tree_Node;
                        The_Key : Multi_ATree.Key_Type);
   --# post Key (N) = The_Key and In_Tree (N);

   --  N may be modified by the following procedures.
   procedure Clear_Tree_Below_Node (N : in out Multi_ATree.Tree_Node);
   --# pre  Is_A_Valid_Tree_Node (N);
   --# post Is_Empty (N);
   --  Removes all nodes from the Tree with Tree_Node values >= N.

private
   type Actual_Node is
      record
         Key   : Multi_ATree.Key_Type;
         Level : Level_Type;
         Left  : Multi_ATree.Tree_Node;
         Right : Multi_ATree.Tree_Node;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Key   => Multi_Atree.Null_Key,
      Level => 0,
      Left  => Multi_Atree.Empty_Node,
      Right => Multi_Atree.Empty_Node);

end SPARK_2005.Multi_ATree.Tree_Abstraction;
