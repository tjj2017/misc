----------------------------------  Atrees  -----------------------------------
--  This package provides an implementaion of Andersson balanced trees using --
--  a Basic_Tree.Tree object to store the nodes of the tree.                 --
--  Andersson Trees are accessed via a type A_Tree object A new Andersson    --
--  tree is established by calling the New_Atree procedure to associate the  --
--  A_Tree object with the Basic_Tree.Tree object.                           --
--  All interactions with the Andersson tree are accomplished using the      --
--  subprograms in this package declaration using the Atree and Basic_Tree   --
--  objects as parameters.                                                   --
--  The host Basic Tree object must be initialized by a call Init_Host_Tree. --
-------------------------------------------------------------------------------

with Basic_Tree;
with Bounded_Stacks;
generic
   type Key_Type is (<>);
   type Value_Type is private;

   Null_Key : in Key_Type;
   Null_Value : in Value_Type;

   Max_Nodes_In_Tree : in Positive;

   Stack_Size : in Positive;

package Atrees with
SPARK_Mode
is
   type Host_Tree is private;  --  Should be a limited type.

   procedure Init_Host_Tree (Host : out Host_Tree);

   type A_Tree is private;

    subtype Key_Count is Natural range 0 .. Max_Nodes_In_Tree;

   --  function Count returns the number of nodes in the Atree.
   function Count (Atree : A_Tree) return Key_Count with
   Inline;

   --  Proof function to assert that the contents of Atree are maintained
   --  within the Host_Tree, Host.
   function In_Host (Atree : A_Tree; Host : Host_Tree) return Boolean with
     Ghost;

   subtype Key_Index is Key_Count;
   --  Logically,each Key in he tree has an index.  Keys are indexed
   --  consecutively such that for all keys in the Atree the value of
   --  Indexed_Key (I + 1) > Indexed_Key (I).  The keys are ordered by index
   --  and there are no duplicate keys.
   function Indexed_Key (Atree : A_Tree;
                         Host  : Host_Tree;
                         Index : Key_Index) return Key_Type with
     Pre => In_Host (Atree, Host) and then Count (Atree) > 0 and then
            (Index > 0 and Index <= Count (Atree)),
     Post => Indexed_Key'Result /= Null_Key,
     Ghost;

   function Ordered (Atree : A_Tree; Host : Host_Tree) return Boolean is
      (for all I in Key_Index range 1 .. Count (Atree) - 1 =>
           Indexed_Key (Atree, Host, I + 1) > Indexed_Key (Atree, Host, I)) with
       Pre => In_Host (Atree, Host),
       Ghost;

   function Populated (Atree : A_Tree; Host : Host_Tree) return Boolean with
     Pre  => In_Host (Atree, Host),
     Ghost;

   function Value_At_Key_Index (Atree : A_Tree;
                                Host  : Host_Tree;
                                Index : Key_Index)
                          return Value_Type with
     Pre => In_Host (Atree, Host) and then Populated (Atree, Host),
     Ghost;

   -----------------------------------------------------------------------

   function Tree_Depth (Atree : A_Tree; Host : Host_Tree) return Natural with
     Pre => In_Host (Atree, Host);

   function Is_Present (Atree : A_Tree; Host : Host_Tree; Key : Key_Type)
                        return Boolean with
     Pre  => In_Host (Atree, Host) and then
             (Populated (Atree, Host) and Ordered (Atree, Host)),
     Post => (if Is_Present'Result then
                  (for some I in Key_Count range 1 .. Count (Atree) =>
                     (Indexed_Key (Atree, Host, I) = Key)));

   function Value (Atree : A_Tree; Host : Host_Tree; Key : Key_Type)
                   return Value_Type with
     Pre  => In_Host (Atree, Host) and then
             (Populated (Atree, Host) and Ordered (Atree, Host)),
     Post => (if Value'Result /= Null_Value then
                (for some I in Key_Count range 1 .. Count(Atree) =>
                     Indexed_Key (Atree, Host, I) = Key and then
                   Value_At_Key_Index (Atree, Host, I) = Value'Result));

   function Equal_Keys (Atree_1, Atree_2 : A_Tree;
                        Host_1, Host_2 : Host_Tree) return Boolean with
   Pre  => (In_Host (Atree_1, Host_1) and In_Host (Atree_2, Host_2)) and then
           (Populated (Atree_1, Host_1) and Populated (Atree_2, Host_2) and
            Ordered (Atree_1, Host_1) and Ordered (Atree_2, Host_2)),
   Post =>  Equal_Keys'Result = (Count (Atree_1) = Count (Atree_2)) and then
               (for all I in Key_Count range 1 .. Count (Atree_1) =>
                  (Indexed_Key (Atree_1, Host_1, I) =
                     Indexed_Key (Atree_2, Host_2, I)));

   function Equal_Keys_And_Values (Atree_1, Atree_2 : A_Tree;
                                   Host_1, Host_2 : Host_Tree)
              return Boolean with
     Pre  => (In_Host (Atree_1, Host_1) and In_Host (Atree_2, Host_2)) and then
             (Populated (Atree_1, Host_1) and Populated (Atree_2, Host_2) and
              Ordered (Atree_1, Host_1) and Ordered (Atree_2, Host_2)),
     Post => Equal_Keys_And_Values'Result =
              (Count (Atree_1) = Count (Atree_2)) and then
               (for all I in Key_Count range 1 .. Count (Atree_1) =>
                 ( Indexed_Key (Atree_1, Host_1, I) =
                     Indexed_Key (Atree_2, Host_2, I) and
                  Value_At_Key_Index (Atree_1, Host_1, I) =
                     Value_At_Key_Index (Atree_2, Host_2, I)));

   procedure New_A_Tree (Atree : out A_Tree; Host : in out Host_Tree) with
   Post => (In_Host (Atree, Host) and Count (Atree) = 0) and then
           Ordered (Atree, Host);

   procedure Insert (Atree     : in out A_Tree;
                     Host      : in out Host_Tree;
                     Key       : Key_Type;
                     Inserted  : out Boolean) with
     Pre  => (In_Host (Atree, Host) and Count (Atree) < Natural'Last) and then
              Ordered (Atree, Host),
     Post => (In_Host (Atree, Host) and Populated (Atree, Host)) and then
              (if Inserted then
                 Count (Atree) = Count (Atree'Old) + 1
               else
                 Count (Atree) = Count (Atree'Old) and
                 Ordered (Atree, Host) and
                (for some I in Key_Count range 1 .. Count (Atree) =>
                     Indexed_Key (Atree, Host, I) = Key));

   procedure Insert_With_Value (Atree         : in out A_Tree;
                                Host          : in out Host_Tree;
                                Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type) with
     Pre  => (In_Host (Atree, Host) and Count (Atree) < Natural'Last) and then
              Ordered (Atree, Host),
     Post => (In_Host (Atree, Host) and Populated (Atree, Host)) and then
             (if Inserted then
                Count (Atree) = Count (Atree'Old) + 1
              else
                Count (Atree) = Count (Atree'Old) and
                Ordered (Atree, Host) and
               (for some I in Key_Count range 1 .. Count (Atree) =>
                  (Indexed_Key (Atree, Host, I) = Key) and
                  (if Inserted then
                     Value_At_Node = Insert_Value and
                       Value (Atree, Host, Key) = Insert_Value
                   else
                     Value_At_Node = Value (Atree, Host, Key) and
                     Value (Atree, Host, Key) =
                       Value (Atree'Old, Host'Old, Key))));


   ------------ Enumerators for Atree depth first traversal ---------------
   type Enumerator is private;

   function Enumerator_Of_Tree (E : Enumerator;
                                A : A_Tree;
                                T : Host_Tree) return Boolean with
     Ghost;

   function Current_Indexed_Key (E: Enumerator;
                                 A: A_Tree;
                                 T : Host_Tree) return Key_Index with
     Pre => Populated (A, T) and then Ordered (A, T),
     Ghost;

   function New_Enumerator (Atree : A_Tree; Host : Host_Tree)
                            return Enumerator with
     Pre  => (In_Host (Atree, Host) and then Populated (Atree, Host)) and then
              Ordered (Atree, Host),
     Post => Enumerator_Of_Tree (New_Enumerator'Result, Atree, Host) and then
             Current_Indexed_Key (New_Enumerator'Result, Atree, Host) = 1;

   procedure Next_Key (E : in out Enumerator;
                       Atree : A_Tree;
                       Host : Host_Tree;
                       Key : out Key_Type) with
     Pre  => (Enumerator_Of_Tree (E, Atree, Host) and then
                In_Host (Atree, Host) and then
                Populated (Atree, Host)) and then Ordered (Atree, Host),
     Post => Enumerator_Of_Tree (E, Atree, Host) and then
             (if Current_Indexed_Key (E, Atree, Host) in
                1 .. Key_Index (Count (Atree) - 1)
              then
                Key = Indexed_Key (Atree, Host,
                                   Current_Indexed_Key (E'Old, Atree, Host)) and
                  Current_Indexed_Key (E, Atree, Host) =
                       Current_Indexed_Key (E'Old, Atree, Host) + 1);

   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Atree     : A_Tree;
                                 Host      : Host_Tree;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type) with
     Pre  => (Enumerator_Of_Tree (E, Atree, Host) and Populated (Atree, Host))
              and then Ordered (Atree, Host),
     Post => Enumerator_Of_Tree (E, Atree, Host) and then
             (Key = Indexed_Key (Atree, Host,
                                 Current_Indexed_Key (E'Old, Atree, Host)) and
                Its_Value = Value_At_Key_Index
                  (Atree, Host,
                   Current_Indexed_Key (E'Old, Atree, Host)) and
                Current_Indexed_Key (E, Atree, Host) =
                  Current_Indexed_Key (E'Old, Atree, Host) + 1);


private
   subtype Node_Index is Natural range 0 .. Max_Nodes_In_Tree;

   package Tree is new Basic_Tree
     (Node_Index => Node_Index,
      Level_Type => Natural,
      Key_Type   => Key_Type,
      Value_Type => Value_Type,
      Null_Key   => Null_Key,
      Null_Value => Null_Value);

   type Host_Tree is new Tree.Tree;

   subtype Valid_Node_Index is Tree.Valid_Node_Index;

   type A_Tree is
      record
         Count : Key_Count;
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

   package Stack is new Bounded_Stacks
     (Element_Type => Valid_Node_Index,
      Stack_Size   => Stack_Size);

   type Enumerator is
      record
         Key_Issue : Key_Index;
          --  A stack to record visited nodes when enumerating.
         Visited : Stack.Stack;
      end record;

end Atrees;
