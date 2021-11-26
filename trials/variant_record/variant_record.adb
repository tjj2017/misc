
--  Example borrowed from https://en.wikibooks.org/wiki/Ada_Programming/Types/record
procedure Variant_Record is

   type My_Int is range 1 .. 100;

   type Traffic_Light is (Red, Yellow, Green);

   type Variant_Record (Option : Traffic_Light) is
      record
	 Location : Natural;
	 case Option is
	    when Red =>
	       Flashing : Boolean := True;
	    when Yellow =>
	       Timeout  : Positive := 1;
	    when Green =>
	       Whatever : Positive := 1;
	 end case;
      end record;

   type A is array (1 .. 5) of My_Int;

   subtype Constrained_VR is Variant_Record (Green);

   type Acc_VR is access all Constrained_VR;

--     Mutable_Traffic_Light   : Variant_Record;
--   Immutable_Traffic_Light : Variant_Record (Option => Yellow);
--     V_Con : aliased Constrained_VR;
--     Acc_TL : Acc_VR := V_Con'Access;
   St : A;
begin
--     Mutable_Traffic_Light   := (Option => Yellow,
--  			       Location => 54,
--  			       Timeout => 5);
--     pragma Assert (Immutable_Traffic_Light.Timeout = 1);
--   Acc_TL.Whatever := 1;
--   pragma Assert (Acc_TL.Whatever = 1);
   null;
end Variant_Record;
