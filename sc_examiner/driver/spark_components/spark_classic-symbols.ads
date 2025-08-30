with GNATCOLL.Symbols,
     Types,
     SPARK_Classic.Pragmas;
use type SPARK_Classic.Pragmas.Mod_Props;
--  # acquire Ada.Containers.Hashed_Maps, Atree;
use type GNATCOLL.Symbols.Symbol;  --  for "=" operator
package SPARK_Classic.Symbols
--  --# own Dictionary;
with Abstract_State => Dictionary
is

   type Symbol_Kind is
     (SS_Null,
      --  Uniquely SPARK Items
      SS_Own,
      SS_Abstract_Type,
      SS_Proof_Function,
      SS_Dependency,
      --  Annotated Ada entities
      SS_Procedure,
      SS_Function,
      SS_Package,
      SS_Entry,
      SS_Task
     );

   subtype Symbol_Id is GNATCOLL.Symbols.Symbol;
   --  Type of a string identifier, or symbol
   No_Symbol : constant Symbol_Id := GNATCOLL.Symbols.No_Symbol;
   --
   function Intern   (S : String) return Symbol_Id;
   function Unintern (S : Symbol_Id) return String;
   --  Conversion between string and symbols named after LISP equivalents

   type Symbol is private;
   Null_Symbol : constant Symbol;

   function Skind (SS : Symbol) return Symbol_Kind;

private
   --  A type for accessing elements of a table.
   --  Used to reference node lists, dependency relations and,
   --  if implemented, node list opitimisations.
   --  For convenience, the range of Node_Id's is used.

   type Table_Ref is range Types.Node_Low_Bound .. Types.Node_High_Bound;

   No_Ref : constant Table_Ref := Table_Ref'First;

   type Store_Node is
      record
         Level : Natural;
         Left  : Table_Ref;
         Right : Table_Ref;
         Value : Types.Node_Id;
      end record;

   --  Initial size of node lists table;
   List_Store_Initial_Size : constant := 1_200;
   --  Growth rate.
   List_Store_Increment    : constant := 100;

   --  Initial size of node lists control table
   Control_Store_Initial_Size : constant := 200;
   Control_Store_Increment    : constant := 100;

--  The Symbol Map is Keyed by Symbol_Id.
--  All kinds of SPARK symbols use the same record format but
--  the use of the record fields varies dependent on the symbol kind.

--     SS_Own_Object_Symbol layout
--        Sym_Kind    : Symbol_Kind;     --  SS_Own
--        Atree_1     : Types.Node_Id;   --  Declaration node of Own Object
--        Atree_2     : Types.Node_Id;   --  Immediately enclosing package.
--        Modifier    : Modifiers;       --  Own object modifier
--        Bool_1      : Boolean;         --  Has Integrity
--        Int_1       : Integer;         --  Integrity Value
--        Bool_2      : Boolean;         --  Has Priority
--        Int_2       : Integer;         --  Priority Value
--        Bool_3      : Boolean;         --  Has Interrupt Handlers
--        L1          : Types.Elist_Id;  --  List of Iterrupt/Handler pairs
--        Bool_4      : Boolean;         --  Is Suspendable
--        Bool_5      : Boolean;         --  Protects
--        L2          : Types.Elist;     --  List of Atree nodes representing
--                                       --  the protected own variables
--      SS_Declare layout
--        Sym_Kind    : Symbol_Kind;     --  SS_Declare
--        Atree_1     : Types.Node_Id;   --  Procedure/Task declaraion node
--        Bool_1      : Boolean;         --  Has delay
--        Bool_2      : Boolean;         --  May Suspend
--        L1          : Types.Elist_Id   --  List of Atree nodes representing
--                                       --  the own variables that the task
--                                       --  may suspend on

   type Symbol is record
      S1              : Symbol_Id;
      S2              : Symbol_Id;
      Ada_Node_1      : Types.Node_Id;
      Ada_Node_2      : Types.Node_Id;
      Sym_Kind        : Symbol_Kind;
   end record;

   Null_Symbol : constant Symbol := Symbol'
     (S1         => No_Symbol,
      S2         => No_Symbol,
      Ada_Node_1 => Types.Empty,
      Ada_Node_2 => Types.Empty,
      Sym_Kind   => SS_Null
     );

end SPARK_Classic.Symbols;
