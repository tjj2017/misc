with SPARK_Classic.Symbols.List_Store;
private package SPARK_Classic.Symbols.Node_Lists
--  --# own In_Progress;
--  --# initializes In_Progress;
with Abstract_State => In_Progress,
     Initializes => In_Progress,
     Initial_Condition => not Building_List
is
   type Node_List is private;

   type Enumerator is limited private;

   function Building_List return Boolean
   --  --# global In_Progress;
   with Global => In_Progress;

   procedure New_List (List : out Node_List)
     --  --# global in out List_Store.Store;
     --  --#           out In_Progress;
   --  --# pre not Building_List;
   --  --# post Building_List;
   with Global => (In_Out => List_Store.Store,
                   Output => In_Progress),
        Pre => not Building_List,
        Post => Building_List;

   procedure Insert (N : Types.Node_Id; List : in out Node_List)
   --  --# global in out List_Sore.Store;
   --  --# pre Building_List;
   --  --# post Building_List;
   with Global => (In_Out => List_Store.Store),
        Pre => Building_List,
        Post => Building_List;

   procedure Save_List (List : in out Node_List)
   --  --# global in out Store;
   --  --#           out In_Progress;
   --  --# pre Building_List;
   --  --# post not Building_List;
   with Global => (In_Out => List_Store.Store,
                   Output => In_Progress),
        Pre => Building_List,
        Post => not Building_List;

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
   --  --# global in List_Store.Store;
     with Global => (Input => List_Store.Store);

   function New_Enumerator (N_List : Node_List
   --  --# global List_Store.Store;
     with Global => List_Store.Store;

   function Next (E : Enumerator) return Types.Node_Id
   -- --#  global List_Store.Store;
   with Global => List_Store.Store;

private
   type Node_List is
      record
         Root : Table_Index;
      end record;

   type Stack is array (Positive range <>) of Table_Index;

   type Index_Stack (Size : Positive) is
      record
         Contents : Stack (1 .. Size);
         Top      : Natural;
      end record;

   type Enumerator is
      record
         Root    : Node_List;
         Index   : Table_Index;
         Visited :
      end record;
end SPARK_Classic.Symbols.Node_Lists;
