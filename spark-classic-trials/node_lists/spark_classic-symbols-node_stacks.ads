with SPARK_Classic.Symbols.Node_Tree,
     GNAT.Dynamic_Tables;
package SPARK_Classic.Symbols.Node_Stacks is
   subtype Element_Type is Node_Tree.Tree_Node;

   type Stack_Type is tagged private;
   subtype Nearly_Natural is Natural range 0 .. Natural'Last - 1;

   function Count    (S : Stack_Type) return Nearly_Natural;
   function Is_Empty (S : Stack_Type) return Boolean
     with Post => Is_Empty'Result = (Count (S) = 0);
   --  --# return Count (S) = 0;
   --  --# post Is_Empty'Result = (Count (S) = 0);

   procedure New_Stack (S : out Stack_Type)
     with Post => Is_Empty (S);

   procedure Push ( S : in out Stack_Type; Value : Element_Type)
     with Post => Count (S) = Count (S)'Old + 1;
   --  --# post Count (S) = Count (S)'Old + 1;

   procedure Pop  ( S : in out Stack_Type; Value : out Element_Type)
     with Pre  => not Is_Empty (S),
     Post => Count (S) = Count (S)'Old - 1;
   --  --# pre not Is_Empty (S);
   --  --# post Count (S) = Count (S)'Old - 1;

   function Top (S : Stack_Type) return Element_Type
     with Pre => not Is_Empty (S);
   --  --# pre  not Is_Empty (S);

   function Predecessor (S : Stack_Type; Pred_Num : Nearly_Natural)
                         return Element_Type
     with Pre => Pred_Num < Count (S);
   --  --# pre Pred_Num < Count (S);
private
   package Dynamic_Stack is new GNAT.Dynamic_Tables
     (Table_Component_Type => Element_Type,
      Table_Index_Type     => Positive,
      Table_Low_Bound      => 1,
      Table_Initial        => 32,
      Table_Increment      => 100);

   type Stack_Type is tagged
      record
         Contents : Dynamic_Stack.Instance;
         Count    : Nearly_Natural;
      end record;
end SPARK_Classic.Symbols.Node_Stacks;
