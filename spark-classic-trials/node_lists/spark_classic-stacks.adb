pragma SPARK_Mode;
package body SPARK_Classic.Stacks is

   -----------
   -- Stack --
   -----------

   package body Stack is

      -----------
      -- Count --
      -----------

      function Count (S : Stack_Type) return Nearly_Natural is
      begin
         return S.Count;
      end Count;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty
        (S : Stack_Type)
         return Boolean
      is
      begin
         return Count (S) = 0;
      end Is_Empty;

      ---------------
      -- New_Stack --
      ---------------

      function New_Stack
         return Stack_Type
      is
         Result : Stack_Type;
      begin
         Dynamic_Stack.Init (Result.Contents);
         Result.Count := 0;
         return Result;
      end New_Stack;

      ----------
      -- Push --
      ----------

      procedure Push
        (Value : Element_Type;
         S : in out Stack_Type)
      is
         Contents : Dynamic_Stack.Table_Type renames S.Contents.Table
           (Dynamic_Stack.First .. Dynamic_Stack.Last (S.Contents));
      begin
         S.Count := S.Count + 1;
         Contents (S.Count) := Value;
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop
        (Value : out Element_Type;
         S : in out Stack_Type)
      is
         Contents : Dynamic_Stack.Table_Type renames S.Contents.Table
           (Dynamic_Stack.First .. Dynamic_Stack.Last (S.Contents));
      begin
         Value := Contents (S.Count);
         S.Count := S.Count - 1;
      end Pop;

      ---------
      -- Top --
      ---------

      function Top
        (S : Stack_Type)
         return Element_Type
      is
         Contents : Dynamic_Stack.Table_Type renames S.Contents.Table
           (Dynamic_Stack.First .. Dynamic_Stack.Last (S.Contents));
      begin
         return Contents (S.Count);
      end Top;

      -----------------
      -- Predecessor --
      -----------------

      function Predecessor
        (Pred_Num : Nearly_Natural;
         S : Stack_Type)
         return Element_Type
      is
         Contents : Dynamic_Stack.Table_Type renames S.Contents.Table
           (Dynamic_Stack.First .. Dynamic_Stack.Last (S.Contents));
      begin
         return Contents (Pred_Num + 1);
      end Predecessor;

   end Stack;

end SPARK_Classic.Stacks;
