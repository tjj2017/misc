----------------------------------  Atrees  -----------------------------------
--  This package provides an implementaion of Andersson balanced trees using  --
--  a Basic_Tree.Tree object to store the nodes of the tree.                 --
--  Anderson Trees are accessed via a type A_Tree object A new Anderson tree --
--  is established by calling the New_Atree procedure to associate the       --
--  A_Tree object with the Basic_Tree.Tree object.                           --
--  All interactions with the Andersson tree are accomplished using the       --
--  subprograms in this package declaration using the Atree and Basic_Tree   --
--  objects as parameters.                                                   --
-------------------------------------------------------------------------------

with Basic_Tree;
with Bounded_Stacks;
generic
   type Key_Type is (<>);
   type Value_Type is private;

   Null_Key : constant Key_Type;
   Null_Value : constant Value_Type;

   Stack_Size : constant Positive;

package Atrees is

   package Tree is new Basic_Tree
     (Node_Index => Node_Index,
      Level_Type => Natural,
      Key_Type   => Key_Type,
      Value_Type => Value_Type,
      Null_Key   => Null_Key,
      Null_Value => Null_Value);

   subtype Host_Tree is Tree.Tree;

   --  function Count returns the number of nodes in the Atree.
   function Count (Atree : A_Tree) return Natural with
   Inline;

   --  Proof function to assert that the contents of Atree are maintained
   --  within the Host_Tree, Host.
   function In_Host (Atree : A_Tree; Host : Host_Tree) return Boolean with
     Ghost;

   --  Logically,each Key in he tree has an index.  Keys are indexed
   --  consecutively such that for all keys in the Atree the value of
   --  Indexed_Key (I + 1) > Indexed_Key (I).  The keys are ordered by index
   --  and there are no duplicate keys.
   function Indexed_Key (Atree : A_Tree;
                         Host  : Host_Tree;
                         Index : Positive) return Key_Type with
     Pre => In_Host (Atree, Host),
     Ghost;

   function Ordered (Atree : A_Tree; Host : Host_Tree) return Boolean is
      (for all I in Positive range 1 .. Count (Atree) - 1 =>
           Indexed_Key (Atree, Host, I + 1) > Indexed_Key (Atree, Host, I)) with
       Pre => In_Host (Atree, Host),
       Ghost;

   function Value_At_Key (Atree : A_Tree; Host : Host_Tree; Key : Key_type)
                          return Value_Type with
     Pre => In_Host (Atree, Host),
     Ghost;

   function Populated (Atree : A_Tree; Host : Host_Tree) return Boolean is
      (Count (Atree) > 0) with
       Pre => In_Host (Atree, Host),
       Ghost;

   -----------------------------------------------------------------------

   function Tree_Depth (Atree : A_Tree; Host : Host_Tree) return Natural with
     Pre => In_Host (Atree, Host);

   function Is_Present (Atree : A_Tree; Host : Host_Tree; Key : Key_Type)
                        return Boolean with
     Pre  => In_Host (Atree, Host) and Populated (Atree, Host) and
             Ordered (Atree, Host),
     Post => (for some I in Positive range 1 .. Count (Atree) =>
                (Indexed_Key (Atree, Host, I) = Key));

   function Value (ATree : A_Tree; Host : Host_Tree; Key : Key_Type)
                   return Value_Type with
     Pre  => In_Host (Atree, Host) and Populated (Atree, Host) and
             Ordered (Atree, Host),
     Post => (for some I in Positive range 1 .. Count(Atree) =>
                (Value_At_Key (Atree, Host, Key) = V);

   function Equal_Keys (Atree_1, Atree_2 : A_Tree;
                        Host_1, Host_2 : Host_Tree) return Boolean with
   Pre  => In_Host (Atree_1, Host_1) and In_Host (Atree_2, Host_2) and
           Populated (Atree_1, Host_1) and Populated (Atree_2, Host_2) and
           Ordered (Atree_1, Host_1) and Ordered (Atree_2, Host_2),
   Post =>  (Count (Atree_1) = Count (Atree_2)) and then
               (for all I in Positive range 1 .. Count (Atree_1) =>
                  (Indexed_Key (Atree_1, Host_1, I) =
                     Indexed_Key (Atree_2, Host_2, I)));

   function Equal_Keys_And_Values (Atree_1, Atree_2 : A_Tree;
                                   Host_1, Host_2 : Host_Tree)
              return Boolean with
   Pre  => In_Host (Atree_1, Host_1) and In_Host (Atree_2, Host_2) and
           Populated (Atree_1, Host_1) and Populated (Atree_2, Host_2) and
           Ordered (Atree_1, Host_1) and Ordered (Atree_2, Host_2),
   Post => (Count (Atree_1) = Count (Atree_2)) and then
              (for all I in Positive range 1 .. Count (Atree_1) =>
                 ( Indexed_Key (Atree_1, Host_1, I) =
                     Indexed_Key (Atree_2, Host_2, I) and
                  Value_At_Key (Atree_1, Host_1,
                                Indexed_Key (Atree_1, Host_1, I)) =
                     Value_At_Key (Atree_2, Host_2,
                                   Indexed_Key (Atree_2, Host_2, I))));

   procedure New_A_Tree (Atree : out A_Tree; Host : in out Host_Tree) with
   Post => In_Host (Atree, Host) and Count (Atree) = 0 and
           Ordered (Atree, Host);

   procedure Insert (Atree     : in out A_Tree;
                     Host      : in out Host_Tree;
                     Key       : Key_Type;
                     Inserted  : out Boolean);
   --# pre In_Host (Atree, Host) and Count (Atree) < Natural'Last and
   --#     Ordered (Atree, Host);
   --# post In_Host (Atree, Host) and Populated (Atree, Host) and
   --#      (Inserted -> ((Count (Atree) = Count (Atree~) + 1))) and
   --#      (not Inserted -> (Count (Atree) = Count (Atree~))) and
   --#      Ordered (Atree, Host) and
   --#      (for some I in Positive range 1 .. Count (Atree) =>
   --#          (Indexed_Key (Atree, Host, I) = Key));

   procedure Insert_With_Value (Atree         : in out A_Tree;
                                Host          : in out Host_Tree;
                                Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type);
   --# pre In_Host (Atree, Host) and Count (Atree) < Natural'Last and
   --#     Ordered (Atree, Host);
   --# post In_Host (Atree, Host) and Populated (Atree, Host) and
   --#      (Inserted -> ((Count (Atree) = Count (Atree~) + 1))) and
   --#      (not Inserted -> (Count (Atree) = Count (Atree~))) and
   --#      Ordered (Atree, Host) and
   --#      (for some I in Positive range 1 .. Count (Atree) =>
   --#          (Indexed_Key (Atree, Host, I) = Key)) and
   --#      (Inserted -> (Value_At_Node = Insert_Value and
   --#                      Value (Atree, Host, Key) = Insert_Value)) and
   --#      (not Inserted -> (Value_At_Node = Value (Atree, Host, Key) and
   --#                          Value (Atree, Host, Key) =
   --#                             Value (Atree~, Host~, Key)));


   ------------ Enumerators for Atree depth first traversal ---------------
   type Enumerator is private;

   --# function Enumerator_Of_Tree (E : Enumerator;
   --#                              A : A_Tree;
   --#                              T : Host_Tree) return Boolean;

   --# function Next_Indexed_Key (E : Enumerator;
   --#                            A : A_Tree;
   --#                            T : Host_Tree) return Natural;
   --# pre Enumerator_Of_Tree (E, A, T);

   function New_Enumerator (Atree : A_Tree; Host : Host_Tree) return Enumerator;
   --# pre In_Host (Atree, Host) and Populated (Atree, Host) and
   --#     Ordered (Atree, Host);
   --# return E => Enumerator_Of_Tree (E, Atree, Host) and
   --#             Next_Indexed_Key (E, Atree, Host) = 1;

   procedure Next_Key (E : in out Enumerator;
                       Atree : A_Tree;
                       Host : Host_Tree;
                       Key : out Key_Type);
   --# pre Enumerator_Of_Tree (E, Atree, Host) and Populated (Atree, Host) and
   --#     Ordered (Atree, Host);
   --# post Enumerator_Of_Tree (E, Atree, Host) and
   --#      Key = Indexed_Key (Atree, Host,
   --#                         Next_Indexed_Key (E~, Atree, Host)) and
   --#      Next_Indexed_Key (E, Atree, Host) =
   --#         Next_Indexed_Key (E~, Atree, Host) + 1;

   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Atree     : A_Tree;
                                 Host      : Host_Tree;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type);
   --# pre Enumerator_Of_Tree (E, Atree, Host) and Populated (Atree, Host) and
   --#     Ordered (Atree, Host);
   --# post Enumerator_Of_Tree (E, Atree, Host) and
   --#      Key = indexed_Key (Atree, Host,
   --#                         Next_Indexed_Key (E~, Atree, Host)) and
   --#      Its_Value = Value_At_Key (Atree, Host, Key) and
   --#      Next_Indexed_Key (E, Atree, Host) =
   --#         Next_Indexed_Key (E~, Atree, Host) + 1;


private
   subtype Node_Index is Basic_Tree.Node_Index;
   subtype Valid_Node_Index is Basic_Tree.Valid_Node_Index;

   type A_Tree is
      record
         Count : Natural;
         Base  : Valid_Node_Index;
         Root  : Node_Index;
      end record;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   --  Stack_Size : Positive

   type Enumerator is
      record
          --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stacks.Stack;
      end record;

end Atrees;
