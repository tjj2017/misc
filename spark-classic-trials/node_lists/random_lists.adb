with Ada.Numerics.Float_Random,
     Types,
     SPARK_Classic.Symbols.Node_Trees,
     Ada.Text_IO;
use Ada.Text_IO;
use type Types.Node_Id;
procedure Random_Lists is
   Number_Of_Lists     : constant := 20;
   Max_Number_Of_Nodes : constant := 10000;
   Allow_Duplicates    : constant Boolean := False;

   subtype List_Index is Positive range 1 .. Number_Of_Lists;
   subtype Node_Index is Positive range 1 .. Max_Number_Of_Nodes;
   type Restricted_List is
      record
         List : SPARK_Classic.Symbols.Node_Trees.Node_List;
         Len  : Node_Index;
      end record;
   type Lists is array (List_Index) of Restricted_List;
   The_Lists : Lists;
   Rev_Lists : Lists;

   type Node_Array is array (Node_Index) of Types.Node_Id;

   Max_Nodes_1 : constant Float := Float (Max_Number_Of_Nodes - 1);
   Max_Node_1  : constant Float := Float (Types.Node_Id'Last - 1);

   Random_Gen : Ada.Numerics.Float_Random.Generator;
   Random_Int   : Integer;
   Random_Index : Node_Index;
   Random_Node  : Types.Node_Id;
   The_Nodes    : Node_Array;
   Rev_Nodes    : Node_Array;
   Enum         : SPARK_Classic.Symbols.Node_Trees.Enumerator;
   Next         : Types.Node_Id;
   Enum_Count   : Natural;
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
   SPARK_Classic.Symbols.Node_Trees.Initialize_Store;
   for I in List_Index loop
      Random_Int :=
        Integer
          (Ada.Numerics.Float_Random.Random (Random_Gen) * Max_Nodes_1) + 1;
      Random_Index := Node_Index (Random_Int);
      The_Lists (I).Len := Random_Index;
      Rev_Lists (I).Len := Random_Index;
      SPARK_Classic.Symbols.Node_Trees.New_List (The_Lists (I).List);
      SPARK_Classic.Symbols.Node_Trees.New_List (Rev_Lists (I).List);
      Random_Node := Types.Node_Id
        (Float'Rounding
           (Ada.Numerics.Float_Random.Random
                (Random_Gen) * Max_Node_1)) + 1;
      for J in Node_Index range 1 .. Random_Index loop
         The_Nodes (J) := Random_Node;
         Rev_Nodes (Random_Index - J + 1) := Random_Node;
         SPARK_Classic.Symbols.Node_Trees.Insert
           (Random_Node, The_Lists (I).List, Inserted);
         if not Inserted then
--              Put_Line ("Duplicate - not inserted: " &
--                          Types.Node_Id'Image (Random_Node));
            null;
         else
            Put (".");
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

      --  Insert nodes in reverse
      for J in Node_Index range 1 .. Random_Index loop
         SPARK_Classic.Symbols.Node_Trees.Insert
           (Rev_Nodes (J), Rev_Lists (I).List, Inserted);
         if not Inserted then
--              Put_Line ("Duplicate - not inserted: " &
--                          Types.Node_Id'Image (Random_Node));
            null;
         else
            Put ("*");
         end if;
      end loop;


--        Put_Line ("List number: " & List_Index'Image (I));
--        Put_Line ("Number of nodes" & Natural'Image (Random_Index));
--        Put_Line ("Depth of tree: " & Integer'Image
--          (SPARK_Classic.Symbols.Node_Trees.Tree_Depth (The_Lists (I).List)));
      Enum := SPARK_Classic.Symbols.Node_Trees.New_Enumerator (The_Lists (I).List);
--        Put_Line ("Insertion count: " & Natural'Image
--                    (SPARK_Classic.Symbols.Node_Trees.Count (The_Lists (I).List)));
--        Put_Line ("The unsorted list " & Positive'Image (I) & ":");
--        for K in Node_Index range 1 .. Random_Index loop
--           Put (Types.Node_Id'Image (The_Nodes (K)) & " ");
--        end loop;
--        New_Line;
--        Put_Line ("The reversed unsorted list " & Positive'Image (I) & ":");
--        for K in Node_Index range 1 .. Random_Index loop
--           Put (Types.Node_Id'Image (Rev_Nodes (K)) & " ");
--        end loop;
--        New_Line;
--  --
--        Enum_Count := 0;
--        Put_Line ("The sorted list " & Positive'Image (I) & ":");
--        loop
--           SPARK_Classic.Symbols.Node_Trees.Next (Enum, Next);
--           exit when Next = Types.Empty;
--           Enum_Count := Enum_Count + 1;
--           Put (Types.Node_Id'Image (Next) & " ");
--        end loop;
--        Put_Line ("Enum count: " & Natural'Image (Enum_Count));
--        New_Line;
--      Put (".");
   end loop;
   New_Line;

   Put_Line ("Check for equality");
   for I in List_Index loop
      for J in List_Index range  I .. List_Index'Last loop
         if I /= J then
            if The_Lists (I).List.Are_Equal (The_Lists (J).List) then
               New_Line;
               Put_Line ("List " & Positive'Image (I) & " List " & Positive'Image (J)
                         & " Equal");
            else
               Put (".");
            end if;
         end if;
      end loop;
   end loop;
   New_Line;

   Put_Line ("Check reversed lists for equality");
   for I in List_Index loop
      if not The_Lists (I).List.Are_Equal (Rev_Lists (I).List) then
         Put_Line ("List and reversed list " & Positive'Image (I) & " are not equal");
      else
         Put (".");
      end if;
   end loop;
   New_Line;

end Random_Lists;
