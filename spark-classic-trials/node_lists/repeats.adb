with Types,
     SPARK_Classic.Symbols.Nodes,
     Ada.Text_IO;
use Ada.Text_IO;
use type Types.Node_Id;
procedure Repeats is
   Number_Of_Lists     : constant := 10;
   Max_Number_Of_Nodes : constant := 100;
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

   type Node_Array is array (Node_Index) of types.Node_Id;
   type Restricted_Array is
      record
         Arr : Node_Array;
         Len : Node_Index;
      end record;

   type Node_List is array (List_Index) of Restricted_Array;

   The_Nodes : constant Node_List := Node_List'
     (Restricted_Array'
        (Arr => Node_Array'(others => 11),
         Len  => 10),
      Restricted_Array'
        (Arr => Node_Array'(others => 22),
         Len  => 20),
     Restricted_Array'
        (Arr => Node_Array'(others => 33),
         Len  => 30),
      Restricted_Array'
        (Arr => Node_Array'(others => 44),
         Len  => 40),
      Restricted_Array'
        (Arr => Node_Array'(others => 55),
         Len  => 50),
      Restricted_Array'
        (Arr => Node_Array'(others => 66),
         Len  => 60),
      Restricted_Array'
        (Arr => Node_Array'(others => 77),
         Len  => 70),
      Restricted_Array'
        (Arr => Node_Array'(others => 88),
         Len  => 80),
      Restricted_Array'
        (Arr => Node_Array'(others => 99),
         Len  => 90),
     Restricted_Array'
        (Arr => Node_Array'(others => 100),
         Len  => 100));

   Enum         : SPARK_Classic.Symbols.Nodes.Enumerator;
   Next         : Types.Node_Id;

begin
   SPARK_Classic.Symbols.Nodes.Initialize_Store;
   for I in List_Index loop
      SPARK_Classic.Symbols.Nodes.New_List (The_Lists (I).List);
      for J in Node_Index range 1 .. The_Nodes (I).Len loop
         SPARK_Classic.Symbols.Nodes.Insert
           (The_Nodes (I).Arr (J), The_Lists (I).List);
      end loop;

      Put_Line ("List number: " & List_Index'Image (I));
      Put_Line ("Depth of tree: " & Integer'Image
        (SPARK_Classic.Symbols.Nodes.Tree_Depth (The_Lists (I).List)));
      Enum := SPARK_Classic.Symbols.Nodes.New_Enumerator (The_Lists (I).List);

      Put_Line ("The unsorted list:");
      for K in Node_Index range 1 .. The_Nodes (I).Len loop
         Put (Types.Node_Id'Image (The_Nodes (I).Arr (K)) & " ");
      end loop;
      New_Line;

      Put_Line ("The sorted list:");
      for K in Node_Index range 1 .. The_Nodes (I).Len loop
         SPARK_Classic.Symbols.Nodes.Next (Enum, Next);
         Put (Types.Node_Id'Image (Next) & " ");
     end loop;
      New_Line;
   end loop;
end Repeats;
