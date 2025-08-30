with Ada.Containers.Hashed_Maps,
     Atree,
     SPARK_Classic.Symbols.List_Store;
package body SPARK_Classic.Symbols
--  --# own Dictionary is SPARK_Classic.Symbols.List_Store.Store,
--  --#                   Table;
with
   Refined_State => (Dictionary => (SPARK_Classic.Symbols.List_Store.Store,
                                    Table))
is
   package Symbol_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Id,  --  Should be Entity_Id
      Element_Type    => Symbol,
      Hash            => GNATCOLL.Symbols.Hash,
      Equivalent_Keys => "=");

   Table : Symbol_Maps.Map;

   Intern_Strings : constant GNATCOLL.Symbols.Symbol_Table_Access :=
     GNATCOLL.Symbols.Allocate;

   function Unintern (S : Symbol_Id) return String is
     (GNATCOLL.Symbols.Get (S).all);

   function Intern (S : String) return Symbol_Id is
     (GNATCOLL.Symbols.Find (Intern_Strings, S));

   function Skind (SS : Symbol) return Symbol_Kind is
   begin
      return SS.Sym_Kind;
   end Skind;

   procedure Set_Symbol_Kind (SK       : Symbol_Kind;
                              Sym_Node : Types.Node_Id;
                              SS       : in out Symbol) is
   begin
      SS.Ada_Node_1 := Sym_Node;
      SS.Sym_Kind := SK;
   end Set_Symbol_Kind;

   procedure Set_Property (Property      : SPARK_Classic.Pragmas.Props;
                           Property_Node : Types.Node_Id;
                           SS            : in out Symbol)
   is
   begin
      case Property is
         when SPARK_Classic.Pragmas.Own_Type =>
            SS.Ada_Node_2 := Property_Node;
         when SPARK_Classic.Pragmas.Integrity =>
            SS.Ada_Node_3 := Property_Node;
         when SPARK_Classic.Pragmas.Priority =>
            SS.Ada_Node_4 := Property_Node;
         when SPARK_Classic.Pragmas.Protects =>
            SS.Ada_Node_5 := Property_Node;
         when SPARK_Classic.Pragmas.Suspendable =>
            SS.Ada_Node_6 := Property_Node;
         when SPARK_Classic.Pragmas.May_Delay  =>
            SS.Ada_Node_7 := Property_Node;
         when SPARK_Classic.Pragmas.Interrupt =>
            SS.Ada_Node_8 := Property_Node;
         when SPARK_Classic.Pragmas.Suspends =>
            SS.Ada_Node_9 := Property_Node;
      end case;
   end Set_Property;

   function Has_Property (Property : SPARK_Classic.Pragmas.Props;
                         SS        : Symbol) return Boolean
   is
      Result : Boolean;
   begin
      case Property is
         when SPARK_Classic.Pragmas.Own_Type =>
            Result := Atree.Present (SS.Ada_Node_2);
         when SPARK_Classic.Pragmas.Integrity =>
            Result := Atree.Present (SS.Ada_Node_3);
         when SPARK_Classic.Pragmas.Priority =>
            Result := Atree.Present (SS.Ada_Node_4);
         when SPARK_Classic.Pragmas.Protects =>
            Result := Atree.Present (SS.Ada_Node_5);
         when SPARK_Classic.Pragmas.Suspendable =>
            Result := Atree.Present (SS.Ada_Node_6);
         when SPARK_Classic.Pragmas.May_Delay  =>
            Result := Atree.Present (SS.Ada_Node_7);
         when SPARK_Classic.Pragmas.Interrupt =>
            Result := Atree.Present (SS.Ada_Node_8);
         when SPARK_Classic.Pragmas.Suspends =>
            Result := Atree.Present (SS.Ada_Node_9);
      end case;
      return Result;
   end Has_Property;

   function Own_Has_Type (SS : Symbol) return Boolean is
   begin
      return Has_Property (SPARK_Classic.Pragmas.Own_Type, SS);
   end Own_Has_Type;

   function Has_Properties (SS : Symbol) return Boolean is
      Result : Boolean := False;
   begin
      for P in SPARK_Classic.Pragmas.Props loop
         Result := Has_Property (P, SS);
         exit when Result;
      end loop;
      return Result;
   end Has_Properties;

   function Has_Non_Type_Properties (SS : Symbol) return Boolean is
      Result : Boolean := False;
   begin
      for P in SPARK_Classic.Pragmas.Props loop
         if P /= SPARK_Classic.Pragmas.Own_Type then
            Result := Has_Property (P, SS);
         end if;
         exit when Result;
      end loop;
      return Result;
   end Has_Non_Type_Properties;

   procedure Insert (Key      : Symbol_Id;
                     New_Item : Symbol;
                     Inserted : out Boolean) is
      Cursor : Symbol_Maps.Cursor;
   begin
      Table.Insert
        (Key      => Key,
         New_Item => New_Item,
         Position => Cursor,
         Inserted => Inserted);
      pragma Assert (if Inserted then Symbol_Maps.Has_Element (Cursor)
                     else True);
   end Insert;

end SPARK_Classic.Symbols;
