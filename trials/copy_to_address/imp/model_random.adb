with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
package body Model_Random is

   G : Generator;

   ------------
   -- Random --
   ------------

   function Random
      return Uniformly_Distributed
   is
   begin
      return Random (G);
   end Random;

end Model_Random;
