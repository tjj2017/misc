package Own_Aspect_11
--  1 --# own V;
--  with Classic_Own => (((Plain => V)))
--  --  2 --# own in V;
--  with Classic_Own => (((External_In => V)))
--  3 --# own out V;
--  with Classic_Own => (((External_Out => V)))
--  4 --# own task V;
--  with Classic_Own => (((Own_Task => V)))
--  5 --# own protected V;
--  with Classic_Own => (((Protected_Own => V)))
--  6 --# own protected in V;
--  with Classic_Own => (((Protected_In => V)))
--  7 --# own protected out V;
--  with Classic_Own => (((Protected_Out => V)))
--  8 --# own V, PV;
--  with Classic_Own => (((Plain => V, Plain => PV)))
--  9 --# own V, PV, State;
--  with Classic_Own => (((Plain => V, Plain => PV, Plain => State)))
--  10 --# own in IP, V, PV, out OP, State, protected in PI;
--  with Classic_Own =>
--    (((External_In => V, Plain => PV, External_Out => OP,
--       Plain => State, Protected_In => PI)))
--  11 --# own V : Integer;
with Classic_Own =>(((Plain => V) with Own_Type => Integer))
--  12 --# own V : Abstract_Type;
--  with Classic_Own => (((Plain => V) with Own_Type => Abstract_Type))
--  13 --# own V, PV : Abstract_Type;
--  with Classic_Own =>
--    (((Plain => V, Plain => PV) with Own_Type => Abstact_Type))
--  14 --# own V, PV : Abstract_Type (integrity => 4));
--  with Classic_Own =>
--    (((Plain => V, Plain => PV) with Own_Type => Abstract_Type, Integrity => 4))
--  15 --# own V, PV : Abstract_Type (Integrity => 4, suspendable);
--  with Classic_Own =>
--    (((Plain => V, Plain => PV) with Own_Type => Abstract_Type,
--     Integrity => 4, Suspendable => True))
--  16 --# own in EI, V, out OP, state, PV, protected in PI : Abstract_Type
--             (Integrity => 4, suspendable);
--  with Classic_Own =>
--    (((External_In => EI, Plain => V, Exteranl_Out => OP,
--       Plain => State, Plain => PV, Protected_In => PI) with
--     Own_Type => Abstract_Type, Integrity => 4, Suspendable => True))
--  17 --# own V, PV : Integer; State : Abstract_Type;
--  with Classic_Own => (((Plain => V, Plain => PV) with Own_Type => Integer),
--                       ((Plain => State) with Own_Type => Abstract_Type))
--  18 --# own V, PV : Integer;
--     --# State : Abstract_Type (Integrity => 4, Suspendable => True) ;
--  with Classic_Own => (((Plain => V, Plain => PV) with Own_Type => Integer),
--                       ((Plain => State) with Own_Type => Abstract_Type,
--                        Integrity => 4, Suspendable => True))
--  19 --# own V; PV; in IP;
--  with Classic_Own => (((Plain => V), (Plain => PV)), (External_In => IP))
--  20 --# own V, State; PV in IP;
--  with Classic_Own =>
--    (((Plain => V, Plain => State), (Plain => PV, External_In => IP)))
is
   V : Integer;
private
   PV : Integer;
end Own_Aspect_11;
