with Ada.Numerics.Float_Random,
     Types,
     SPARK_Classic.Symbols.Nodes,
     Ada.Text_IO;
use Ada.Text_IO;
use type Types.Node_Id;
procedure Random_Lists is
   Number_Of_Lists     : constant := 15;
   Max_Number_Of_Nodes : constant := 30;
   Allow_Duplicates    : constant Boolean := True;

   subtype List_Index is Positive range 1 .. Number_Of_Lists;
   subtype Node_Index is Positive range 1 .. Max_Number_Of_Nodes;
   type Restricted_List is
      record
         List : SPARK_Classic.Symbols.Nodes.Node_List;
         Len  : Node_Index;
      end record;
   type Lists is array (List_Index) of Restricted_List;
   The_Lists : Lists;

   type Node_Array is array (Node_Index) of Types.Node_Id;

   Max_Nodes_1 : constant Float := Float (Max_Number_Of_Nodes - 1);
   Max_Node_1  : constant Float := 29.00; --  Float (Types.Node_Id'Last - 1);

   Random_Gen : Ada.Numerics.Float_Random.Generator;
   Random_Index : Node_Index;
   Random_Node  : Types.Node_Id;
   The_Nodes    : Node_Array;
   Enum         : SPARK_Classic.Symbols.Nodes.Enumerator;
   Next         : Types.Node_Id;
   Inserted     : Boolean;

   function Is_Duplicate (N : Types.Node_Id; A : Node_Array; Len : Natural)
                          return Boolean
   is
      Result : Boolean := False;
   begin
      for I in Node_Index range 1 .. Len loop
         if A (I) = N then
            Result := True;
            exit;
         end if;
      end loop;
      return Result;
   end Is_Duplicate;

begin
   SPARK_Classic.Symbols.Nodes.Initialize_Store;
   for I in List_Index loop
      Random_Index := Node_Index (Float'Rounding
                                  (Ada.Numerics.Float_Random.Random (Random_Gen) * Max_Nodes_1)) + 1;
      The_Lists (I).Len := Random_Index;
      SPARK_Classic.Symbols.Nodes.New_List (The_Lists (I).List);
      Random_Node := Types.Node_Id
        (Float'Rounding
           (Ada.Numerics.Float_Random.Random
                (Random_Gen) * Max_Node_1)) + 1;
      for J in Node_Index range 1 .. Random_Index loop
         The_Nodes (J) := Random_Node;
         SPARK_Classic.Symbols.Nodes.Insert
           (Random_Node, The_Lists (I).List, Inserted);
         if not Inserted then
            Put_Line ("Duplicate - not inserted: " &
                        Types.Node_Id'Image (Random_Node));
         end if;
         loop
            Random_Node := Types.Node_Id
              (Float'Rounding
                 (Ada.Numerics.Float_Random.Random
                      (Random_Gen) * Max_Node_1)) + 1;
            exit when Allow_Duplicates or else
              not Is_Duplicate (Random_Node, The_Nodes, J);
         end loop;
      end loop;

      Put_Line ("List number: " & List_Index'Image (I));
      Put_Line ("Depth of tree: " & Integer'Image
        (SPARK_Classic.Symbols.Nodes.Tree_Depth (The_Lists (I).List)));
      Enum := SPARK_Classic.Symbols.Nodes.New_Enumerator (The_Lists (I).List);

      Put_Line ("The unsorted list:");
      for K in Node_Index range 1 .. Random_Index loop
         Put (Types.Node_Id'Image (The_Nodes (K)) & " ");
      end loop;
      New_Line;

      Put_Line ("The sorted list:");
      loop
         SPARK_Classic.Symbols.Nodes.Next (Enum, Next);
         exit when Next = Types.Empty;
         Put (Types.Node_Id'Image (Next) & " ");
      end loop;
      New_Line;
   end loop;
end Random_Lists;
