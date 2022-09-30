with SPARK_Classic.Atrees;
package SPARK_Classic.Symbols.Node_Trees
with Abstract_State => Store
--  --# own Store;
is
   type Node_List is tagged private;

   type Enumerator is private;

   procedure Initialize_Store
     with Global => (Output => Store),
          Post   => not Building_List;

   function Building_List return Boolean
   with Global => Store;
   --  --# global Store;

   function Empty_List (List : Node_List) return Boolean;

   procedure New_List (List : out Node_List)
   with Global => (In_Out => Store),
        Pre => not Building_List,
        Post => Building_List;
   --  --# global in out Store;
   --  --# pre not Building_List;
   --  --# post Building_List and Empty_List (N_List);

   procedure Insert (N : Types.Node_Id;
                     List : in out Node_List;
                     Inserted : out Boolean)
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => Building_List;
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List and not Empty_List (N_List);

   procedure Save_List (List : in out Node_List)
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => not Building_List;
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post not Building_List;

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
     with Global => (Input => Store);
   --  --# global in Store;

   function Tree_Depth (N_List : Node_List) return Natural
     with Global => (Input => Store);
   --  -- global in Store;

   function New_Enumerator (N_List : Node_List) return Enumerator
     with Global => (Input => Store),
          Pre    => not Empty_List (N_List);
   --  --# global in Store;
   --  --# pre not Empty_List (N_List);

   procedure Next (E : in out Enumerator; Next_Value: out Types.Node_Id)
   with Global => (Input => Store);
   -- --#  global in Store;

private
   type Null_Record is null record;
   Null_Value : constant Null_Record := (null record);

   package Atrees is new SPARK_Classic.Atrees
     (Key_Type   => Types.Node_Id,
      Value_Type => Null_Record,
      Null_Value => Null_Value);

   type Node_List is tagged record
      Tree   : Atrees.A_Tree;
      Length : Natural;
   end record;

   type Enumerator is record
      E : Atrees.Enumerator;
   end record;

end SPARK_Classic.Symbols.Node_Trees;
