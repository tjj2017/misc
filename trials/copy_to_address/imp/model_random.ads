package Model_Random is
   subtype Uniformly_Distributed is Float;
   function Random return Uniformly_Distributed;
   --  with Annotate => (ASVAT, Nondet);
end Model_Random;
